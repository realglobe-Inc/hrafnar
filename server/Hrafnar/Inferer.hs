{-|
Description : Type inferer
Module      : Hrafnar.Recipe.Inference
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}
module Hrafnar.Inferer
  ( Scheme (..)
  , TEnv
  , infer
  ) where

import           Hrafnar.Annotation
import           Hrafnar.AST
import           Hrafnar.Exception
import           Hrafnar.Types

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Lens           hiding (Context, List)
import           Control.Monad.RWS
import           Control.Monad.Writer
import qualified Data.Graph             as GR
import qualified Data.List              as L
import qualified Data.Map               as MA
import           Data.Maybe
import qualified Data.Set               as SE

import           Debug.Trace

type Subst = MA.Map TV Type

data Constraint
  = EqConst Type Type
  | ExplConst Type Scheme
  | ImplConst Type (SE.Set TV) Type
  deriving (Show, Eq, Ord)

type TEnv = MA.Map Name Scheme

-- | Monad for inference.
type Infer a  = forall m. MonadThrow m => RWST TEnv () Int m a

type Answer = (Type, [Constraint])

class Substitutable a where
  apply :: Subst -> a -> a
  free :: a -> SE.Set TV

instance Substitutable TV where
  apply s a = tv
    where
      (TyVar tv) = MA.findWithDefault (TyVar a) a s
  free = SE.singleton

instance Substitutable Type where
  apply _ a@TyCon{}     = a
  apply s t@(TyVar a)   = MA.findWithDefault t a s
  apply s (TyFun t1 t2) = TyFun (apply s t1) (apply s t2)
  apply s (TyTuple ts)  = TyTuple (apply s ts)
  apply s (TyList t)    = TyList (apply s t)

  free TyCon{}       = SE.empty
  free (TyVar a)     = SE.singleton a
  free (TyFun t1 t2) = SE.union (free t1) (free t2)
  free (TyTuple ts)  = free ts
  free (TyList t)    = free t

instance Substitutable Scheme where
  apply s (Forall as t)   = Forall as $ apply s' t
    where
      s' = foldr MA.delete s as
  free (Forall as t) = SE.difference (free t) (SE.fromList as)

instance Substitutable Constraint where
  apply s (EqConst t1 t2)      = EqConst (apply s t1) (apply s t2)
  apply s (ExplConst t sc)     = ExplConst (apply s t) (apply s sc)
  apply s (ImplConst t1 ms t2) = ImplConst (apply s t1) (apply s ms) (apply s t2)

  free (EqConst t1 t2)      = SE.union (free t1) (free t2)
  free (ImplConst t1 ms t2) = SE.union (free t1) (SE.intersection (free ms) (free t2))
  free (ExplConst t s)      = SE.union (free t) (free s)

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  free   = foldr (SE.union . free) SE.empty

instance (Ord a, Substitutable a) => Substitutable (SE.Set a) where
  apply = SE.map . apply
  free = foldr (SE.union . free) SE.empty

instance Substitutable TEnv where
  apply s = MA.map (apply s)
  free env = free $ MA.elems env

-- | Infer a type of the @Expr@
infer :: MonadThrow m => TEnv -> Expr -> m Scheme
infer env expr = view _1 <$> evalRWST m env 0
  where
    m = do
      (t, cs) <- inferExpr expr
      s <- solve cs
      normalize . generalize $ apply s t

-- | Get a fresh type variable.
fresh :: Infer Type
fresh = do
    s <- get
    put $ s + 1
    pure . TyVar . TV $ show s

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  s <- MA.fromList <$> mapM (\a -> (a,) <$> fresh) as
  pure $ apply s t


scoped :: TEnv -> Infer a -> Infer a
scoped env m = do
  let sc = MA.union env
  local sc m

fvsExpr :: MonadThrow m => Expr -> m (SE.Set Name)
fvsExpr = execWriterT . go
  where
    go = \case
      At _ Lit{} -> pure () -- TODO: consider list and tuple
      At _ Do{} -> pure () -- TODO: consider list and tuple
      At _ (Var x) -> tell . SE.singleton $ x
      At _ (Lambda x e) -> censor (SE.delete x) $ go e
      At _ (Apply e1 e2) -> go e1 >> go e2
      At _ (If cond e1 e2) -> go cond >> go e1 >> go e2
      At _ (Case expr bs) -> do
        go expr
        forM_ bs $ \(p, e) -> do
          fvs <- fvsPat p
          censor (SE.\\ fvs) $ go e
      At _ (Let ds e') -> do
        (vars, defs) <- unzip . view _1 <$> scanDecls ds
        let bound = SE.fromList vars
        _ <- censor (SE.\\ bound) $ mapM go (e':fmap (^. _1) defs)
        pure ()

fvsPat :: MonadThrow m => Pat -> m (SE.Set Name)
fvsPat = execWriterT . go
  where
    go = \case
      At _ (PVar n) -> tell . SE.singleton $ n
      At _ (PLit _) -> pure ()
      At _ (PCon _ ps) -> mapM_ go ps
      At _ (PCons p p') -> go p >> go p'
      At _ PWildcard -> pure ()

inferExpr :: Expr -> Infer Answer
inferExpr = \case
  At _ (Lit (Int _)) -> pure (tyInt, [])
  At _ (Lit (Bool _)) -> pure (tyBool, [])
  At _ (Lit (Char _)) -> pure (tyChar, [])
  At _ (Lit (String _)) -> pure (tyString, [])

  At _ (Lit (Tuple xs)) -> do
    (ts, cs) <- L.unzip <$> mapM inferExpr xs
    pure (TyTuple ts, L.concat cs)

  At _ (Lit (List _)) -> undefined -- TODO: not implement yet

  At _ (Var x) -> do
    env <- ask
    case MA.lookup x env of
      Nothing ->
        throwString "unbound variable"
      Just s -> do
        t <- instantiate s
        pure (t, [])

  At _ (Lambda x e) -> do
    tv <- fresh
    (t, c) <- scoped (MA.singleton x $ Forall [] tv) (inferExpr e)
    pure (TyFun tv t, c)

  At pos (Apply e1 e2) -> do
    (t1, c1) <- inferExpr e1
    (t2, c2) <- inferExpr e2
    tv <- fresh
    pure (tv, c1 <> c2 <> [EqConst t1 (TyFun t2 tv)])

  At _ (If cond tr fl) -> do
    (t1, c1) <- inferExpr cond
    (t2, c2) <- inferExpr tr
    (t3, c3) <- inferExpr fl
    pure (t2, c1 <> c2 <> c3 <> [EqConst t1 tyBool, EqConst t2 t3])

  At _ (Case e bs) ->
    inferCase e bs

  At _ Do{} -> undefined -- TODO: not implement yet

  At _ (Let ds e') -> do
    (binds, dataDecls) <- scanDecls ds
    g <- sequence [SE.toList <$> fvsExpr e >>= (\fs -> pure ((n, (e, t)), n, fs)) | (n, (e, t)) <- binds]
    let iss = fmap GR.flattenSCC . GR.stronglyConnComp $ g
    newEnv <- dataDeclsToEnv dataDecls
    scoped newEnv $ inferBinds iss e'

inferBinds :: [[(Name, (Expr, Maybe Type))]] -> Expr -> Infer Answer
inferBinds binds e = go binds
  where
    go [] = inferExpr e
    go (bs:bss) = do
      env <- ask
      newEnv <- MA.fromList <$> traverse (\(n, _) -> (fresh <&> generalize) <&> (n,)) bs
      answers <- forM bs $ \b -> do
        (t, cs) <- scoped newEnv (inferExpr (b ^. _2 . _1 ))
        case b ^. _2 . _2 of
          Just t' -> pure (t, EqConst t t': cs)
          Nothing -> pure (t, cs)
      su <- solve (mconcat $ fmap (^. _2) answers)
      let tvs = MA.keysSet newEnv
          scs = MA.fromList $ zipWith (\b s -> (b ^. _1, s)) bs $ fmap (\(t, _) -> generalize (apply su t)) answers
      scoped scs $ local (apply su) (inferBinds bss e)

inferCase :: Expr -> [(Pat, Expr)] -> Infer Answer
inferCase expr bs = do
  ans0@(t0, cs0) <- inferExpr expr
  answers <- forM bs $ \(pat, e) -> case pat of
    At _ (PVar x) -> do
      tv <- fresh
      (t, cs) <- scoped (MA.singleton x $ Forall [] tv) (inferExpr e)
      pure (t, EqConst t0 tv : cs)

    At _ (PLit l) -> do
      (t, cs) <- inferExpr $ withDummy (Lit l)
      (t', cs') <- inferExpr e
      pure (t', EqConst t0 t : cs <> cs')

    At _ (PCon n ps) -> do
      env <- ask
      dc <- instantiate =<<
            case  MA.lookup n env of
              Just x  -> pure x
              Nothing -> throwString "data constructor not found"
      (anssPat, fvs) <- L.unzip <$> mapM (inferPat ans0) ps
      let dupFvs = L.filter ((>1) . L.length) . L.group . fmap (^. _1) $ mconcat fvs
      unless (L.null dupFvs) $ throwString "duplicated pattern variables"
      (t, cs) <- foldM applyAnswer (dc, []) anssPat
      -- FIXME: same variables are able to exist.
      (t', cs') <- scoped (MA.fromList $ mconcat fvs) $ inferExpr e
      pure (t', EqConst t0 t: cs0 <> cs  <> cs')

    At _ PCons{} -> throwString "invalid pattern"

    At _ PWildcard -> pure ans0

  pure $ foldr1 (\(t, cs) (t', cs') -> (t, EqConst t t': cs <> cs'))  answers

applyAnswer :: Answer -> Answer -> Infer Answer
applyAnswer (TyFun t1 t2, csa) (tv@TyVar{}, csb) =
  pure (t2, EqConst tv t1 : csa <> csb)
applyAnswer (TyFun t1 t2, csa) (t3, csb)
  | t1 == t3 = pure (t2, csa <> csb)
  | otherwise = throwString "type unmatched"
applyAnswer _ _ = throwString "can't apply to value"

inferPat :: Answer -> Pat -> Infer (Answer, [(Name, Scheme)])
inferPat ans0@(t0, cs0) pat = case pat of
  At _ (PVar n) -> do
    tv <- fresh
    pure ((tv, cs0), [(n, Forall [] tv)])

  At _ (PLit l) -> do
    _ <- fresh
    (t, cs) <- inferExpr $ withDummy (Lit l)
    pure ((t, cs <> cs0), [])

  At _ (PCon n ps) -> do
    env <- ask
    dc <- instantiate =<<
      case  MA.lookup n env of
        Just x  -> pure x
        Nothing -> throwString "data constructor not found"
    (anssPat, fvs) <- L.unzip <$> mapM (inferPat ans0) ps
    let dupFvs = L.filter ((>1) . L.length) . L.group . fmap (^. _1) $ mconcat fvs
    unless (L.null dupFvs) $ throwString "duplicated pattern variables"
    (t, cs) <- foldM applyAnswer (dc, []) anssPat
    pure ((t, cs), mconcat fvs)

  At _ PCons{} -> throwString "invalid pattern"

  At _ PWildcard -> do
    tv <- fresh
    pure ((tv, []), [])


scanDecls :: MonadThrow m => [Decl] -> m ([(Name, (Expr, Maybe Type))], [DataDecl])
scanDecls decls = do
  let (exprs, types, dats) = splitDecls decls ([], [], [])
      dupExprs = filter ((>1) . length) $ L.group (fmap (view _1) exprs)
      dupTypes = filter ((>1) . length) $ L.group (fmap (view _1) types)
      dupData = filter ((>1) . length) $ L.group (fmap (view _1) dats)
  unless (null dupExprs && null dupTypes && null dupData) $ throwString "duplicated declaration"
  unless (L.null $ fmap (^. _1) types L.\\ fmap (^. _1) exprs) $ throwString "lacking binding"
  pure (fmap (\(x, e) -> (x, (e, L.lookup x types))) exprs, dats)

dataDeclsToEnv :: MonadThrow m => [DataDecl] -> m TEnv
dataDeclsToEnv decls = do
  let dup = L.filter ((>1) . length) . L.group .  mconcat $ fmap (view $ _2 . _1) decls
  unless (null dup) $ throwString "duplicated data constructors"
  pure . MA.unions $ fmap (\(_, (ns, ds)) -> MA.fromList $ fmap (\(n, t) -> (n, Forall (fmap TV ns) t)) ds) decls

normalize :: MonadThrow m => Scheme -> m Scheme
normalize (Forall _ t) =
  let
    normtype :: MonadThrow m => Type -> m Type
    normtype (TyFun a b) = liftA2 TyFun (normtype a) (normtype b)
    normtype (TyTuple as) =  TyTuple <$> mapM normtype as
    normtype (TyList _) = undefined -- TODO: not implement yet
    normtype (TyCon name ts) = TyCon name <$> mapM normtype ts
    normtype (TyVar a) =
      case L.lookup a ts of
        Just x  -> pure . TyVar $ TV x
        Nothing -> throwString "type variable not found"

    ts = L.zip (L.nub $ fv t) (show <$> [0..])

    fv (TyVar a)    = [a]
    fv (TyFun a b)  = fv a <> fv b
    fv (TyCon _ ts) = L.concatMap fv ts
    fv (TyTuple as) = L.concatMap fv as
    fv (TyList a)   = fv a
  in do
    t' <- normtype t
    pure $ Forall (fmap (TV . snd) ts) t'

generalize :: Type -> Scheme
generalize t = Forall as t
  where
    as = SE.toList $ free t

solve :: [Constraint] -> Infer Subst
solve [] = pure MA.empty

solve (EqConst t1 t2:cs) = do
  su1 <- unify t1 t2
  su2 <- solve (apply su1 cs)
  pure $ MA.union su2 su1

solve (ImplConst t1 ms t2:cs) =
  solve $ ExplConst t1 (generalize t2) : cs

solve (ExplConst t s: cs) = do
  s' <- instantiate s
  solve $ EqConst t s' : cs



unify :: MonadThrow m => Type -> Type -> m Subst
unify t1 t2 | t1 == t2 = pure MA.empty
unify (TyVar v) t = bind v t
unify t (TyVar v) = bind v t
unify (TyCon n1 ts1) (TyCon n2 ts2) = unless (n1 == n2) (throwString "failed inification") >>  unifyMany ts1 ts2
unify (TyFun t1 t2) (TyFun t3 t4) = unifyMany [t1, t2] [t3, t4]
unify t1 t2 = throwString $ "failed unification: " <> show t1 <> ", " <> show t2

unifyMany :: MonadThrow m => [Type] -> [Type] -> m Subst
unifyMany [] [] = pure MA.empty
unifyMany (t1 : ts1) (t2 : ts2) = do
  s1 <- unify t1 t2
  s2 <- unifyMany (fmap (apply s1) ts1) (fmap (apply s1) ts2)
  pure $ MA.union s1 s2
unifyMany t1 t2 = throwString $ "unification mismatch: " <> show t1 <> " " <> show t2

bind :: MonadThrow m => TV -> Type -> m Subst
bind a t | t == TyVar a = pure MA.empty
         | occurs a t = throwString $ "occurs check failed: " <> show a <> " " <> show t
         | otherwise = pure $ MA.singleton a t

occurs :: TV -> Type -> Bool
occurs a t = SE.member a (free t)
