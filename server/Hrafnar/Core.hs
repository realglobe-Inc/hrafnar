{-|
Description : Compiler and evaluater
Module      : Hrafnar.Core
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}

module Hrafnar.Core
  ( Recipe
  , Effect(..)
  , ValTable
  , ExprTable
  , Situation
  , LocalState
  , Transaction
  , initialState
  , initSituation
  , defaultSituation
  , Eval
  , Value(..)
  , VEnv
  , compile
  , compileData
  , evalMain
  , evalExpr
  , link
  , update
  , combI
  , module Hrafnar.AST
  , (!)
  ) where

import           Hrafnar.Annotation
import           Hrafnar.AST
import           Hrafnar.Event
import           Hrafnar.Exception
import           Hrafnar.Types
import           Hrafnar.Value

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Lens           hiding (Const, Context, List)
import           Control.Monad.RWS
import           Data.Extensible        hiding (State)
import qualified Data.List as L
import Data.Foldable (foldlM)
import qualified Data.Map               as MA
--import           Debug.Trace

type Recipe = Record
  '[ "initial" >: ExprTable
   , "cookbook" >: [(String, Expr)]
   ]


-- | Map of @Val@
type ValTable = MA.Map String Value

-- | Map of @Expr@
type ExprTable = MA.Map String Expr

type Situation = Record
  '[ "event" >: Event
   , "context" >: ExprTable
   , "implant" >: MA.Map String Value
   , "effects" >: ValTable
   ]

type LocalState = Record
  '[ "count" >: Int
   , "depth" >: Int
   , "venv" >: VEnv
   ]

type VEnv = MA.Map Name Value


defaultSituation :: Situation
defaultSituation = initSituation MA.empty (#init # ())

initSituation :: ExprTable -> Event -> Situation
initSituation c e =
  #event @= e <:
  #context @= c <:
  #implant @= MA.empty <:
  #effects @= MA.empty <:
  nil

initialState :: LocalState
initialState =
  #count @= 0 <:
  #depth @= 0 <:
  #venv @= MA.empty <:
  nil

type Transaction = Endo (ExprTable, [Effect Event])

-- | Eval monad. @Situation@ can be only read and @Transaction@ can be written.
--   To be both read and written is @LocalState@.
type Eval a = forall m. MonadThrow m => RWST Situation Transaction LocalState m a


-- | compile expression to intermidiate representation.
compile :: Expr -> Eval Value
compile (At _ expr) = compile' expr
  where
    compile' = \case
      Var name -> pure $ VVar name
      Lit l -> compileLit l
      Lambda name expr0 -> abstract name <$> compile expr0
      Apply fun arg -> liftA2 VApp (compile fun) (compile arg)
      Let ds e ->
        let
          letInBind [] expr' = compile expr'
          letInBind (At _ (ExprDecl n e'):ds') expr' =
            liftA2 VApp (abstract n <$> (letInBind ds' expr')) (compile e')
          letInBind (At _ (TypeAnno{}):ds') expr' = letInBind ds' expr'
        in
          letInBind ds e
      If cond x y -> foldl VApp (VVar "if") <$> traverse compile [cond, x, y]
      Do exps -> traverse compile exps >>= \case
        [] -> pure $ VTuple []
        xs -> pure $ last xs
      Case expr pats -> do
        v <- compile expr
        compilePat v pats

    compileLit :: Lit -> Eval Value
    compileLit (Bool b)     = pure $ VBool b
    compileLit (Int i)      = pure $ VInt i
    compileLit (Tuple exps) = VTuple <$> traverse compile exps
    compileLit _            = throw FailCompileLit


    compilePat :: Value -> [(Pat, Expr)] -> Eval Value
    compilePat v [(p, expr)] = do
      let (_, sbst) = eqPattern v p
      lastBranch <- compile expr
      pure $ bindInCase sbst lastBranch
    compilePat v ((p, expr):rest) = do
      let (isMatch, sbst) = eqPattern v p
      thenBranch <- compile expr
      elseBranch <- compilePat v rest
      pure $ VApp (VApp (VApp if_ isMatch) (bindInCase sbst thenBranch)) elseBranch

    bindInCase :: [(String, Value)] -> Value -> Value
    bindInCase [] v = v
    bindInCase [(n, v')] v = VApp (abstract n v) v'
    bindInCase ((n, v'):rest) v = VApp (abstract n (bindInCase rest v)) v'


if_ :: Value
if_ = VLam $ \(VBool b) -> VLam $ \x -> VLam $ \y -> if b then x else y

eqPattern :: Value -> Pat -> (Value, [(String, Value)])
eqPattern v pat = eqPattern' (v, pat)
  where
    eqPattern' :: (Value, Pat) -> (Value, [(String, Value)])
    eqPattern' (v, (At _ p)) = case p of
      PVar n -> (VBool True, [(n, v)])
      PLit (Int i)-> (eq_ (VInt i) v , [])
      PLit (Bool b) -> (eq_ (VBool b) v , [])
      PLit (Char c ) -> (eq_ (VChar c) v, [])
      PLit (String s) -> (eq_ (VString s) v, [])
      PWildcard -> (VBool True, [])
      PCon pname pats ->
        foldl (accumlate v) (VBool True, []) $ zip [0..] pats
      _ -> (VBool False, [])
    accumlate v (isMatch, sbst) (index, pat) =
      let (isMatch', sbst') = eqPattern' (VApp (getConsParamByIndex index) v, pat) in (and_ isMatch isMatch', sbst <> sbst')

eq_ :: Value -> Value -> Value
eq_ v1 v2 = VApp (VApp (VVar "eq") v1) v2

and_ :: Value -> Value -> Value
and_ v1 v2 = VApp (VApp f v1) v2
  where
    f = VLam $ \(VBool b1) -> VLam $ \(VBool b2) -> VBool (b1 && b2)

getConsParamByIndex :: Int -> Value
getConsParamByIndex n = VLam $ \(VCon _ values) -> values !! n


compileData :: DataDecl -> [(Name, Value)]
compileData (_, (_, conss)) = fmap (\(n, _) -> (n, VCon n [])) conss

compilePat :: (Pat, Expr) -> Eval (VPat, Value)
compilePat (pat, expr) = compilePat' pat >>= makePair
  where
    makePair vp = do
      v <- compile expr
      pure (vp, v)

    compilePat' (At _ pat) = case pat of
      PVar n      -> pure $ VPVar n
      PLit l      -> lit l
      PCon n pats -> VPCon n <$> traverse compilePat' pats
      PCons{}     -> undefined
      PWildcard   -> pure VPWildcard
    lit = \case
      Int i -> pure $ VPInt i
      Bool b -> pure $ VPBool b
      Char c -> pure $ VPChar c
      String s -> pure $ VPString s
      Tuple t -> VPTuple <$> traverse compile t
      List l -> VPList <$> traverse compile l



abstract :: Name -> Value -> Value
abstract x (VApp fun arg) =  combS (abstract x fun) (abstract x arg)
abstract x (VVar n)       | x == n = combI
abstract _ k              = combK k

combI :: Value
combI = VLam id

combK :: Value -> Value
combK = VApp (VLam $ \x -> VLam $ const x)

combS :: Value -> Value -> Value
combS f = VApp (VApp c f)
  where
    c = VLam $ \f' -> VLam $ \g -> VLam $ \x -> f'!x!(g!x)

infixl 0 !
(!) :: Value -> Value -> Value
VLam f ! x = f x
_ ! _ = undefined

-- | replace variable with value
link :: Value -> Eval Value
link (VApp var@VVar{} arg) = liftA2 VApp (link var) (link arg) >>= link
link (VApp (VCon n vs) arg) = do
  arg' <- link arg
  pure $ VCon n (vs <> [arg'])
link (VApp app@VApp{} arg) = liftA2 VApp (link app) (link arg) >>= link
link (VApp fun arg) = liftA2 (!) (link fun) (link arg)
link (VVar name) = MA.lookup name <$> gets (view #venv) >>= \case
  Just v -> link v
  _ -> throw $ VariableNotDeclared name
link (VEff ef) = do
  tell $ Endo (\(c, efs) -> (c, ef : efs))
  pure $ VTuple []
link e = pure e

evalExpr :: Expr -> Eval Value
evalExpr expr = compile expr >>= link

-- | compile all decls and link main
evalMain :: [Decl] -> Eval Value
evalMain decls = do
  let (exprs, _, dats) = splitDecls decls ([], [], [])
  values <- MA.fromList <$> traverse (\(n, e) -> compile e >>= pure . (n,)) exprs
  vars <- gets $ view #venv
  let dataEnv = MA.fromList . join $ fmap compileData dats
  modify $ #venv .~ MA.unions [vars, values, dataEnv]

  case MA.lookup "main" values of
    Just v  -> link v
    Nothing -> throw $ VariableNotDeclared "main"


update :: MonadThrow m => Expr -> Situation -> m (ExprTable, [Effect Event])
update cookbook situation = do
  (_, transaction) <- evalRWST (evalExpr cookbook) situation initialState
  pure $ appEndo transaction (situation ^. #context, [])
