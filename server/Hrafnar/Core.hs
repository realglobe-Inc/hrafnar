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
      Case expr pats -> liftA2 VCase (compile expr) (traverse compilePat pats)

    compileLit :: Lit -> Eval Value
    compileLit (Bool b)     = pure $ VBool b
    compileLit (Int i)      = pure $ VInt i
    compileLit (Tuple exps) = VTuple <$> traverse compile exps
    compileLit _            = throw FailCompileLit


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

-- | apply Value to Value(VLam)
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
link (VCase v pats) = do
  v' <- link v
  consumePats v' pats
link e = pure e

consumePats :: Value -> [(VPat, Value)] -> Eval Value
consumePats _ [] = throwString "There is no matched pattern" -- temp error
consumePats value ((pat, patv):xs) = do
  let (isMatch, sbst) = solve pat value []
  if isMatch
    then
      let f (n, v') acc = VApp (abstract n acc) v' in
      link $ foldr f patv (reverse sbst)
    else consumePats value xs
  where
    conName (VCon n _) = n
    conValues (VCon _ vs) = vs
    -- |
    -- judge whether given pattern matches given value
    -- and return local variable name and correspondsant value
    solve :: VPat -> Value -> [(String, Value)] -> (Bool, [(String, Value)])
    solve pat v acc = case pat of
      VPVar n       -> (True, (n, v) : acc)
      VPCon n pats' | n == conName v ->
        let f (p, v) (isMatch, sbst) =
              let (isMatch', sbst') = solve p v sbst in (isMatch && isMatch', sbst')
        in
        foldr f (True, acc) (zip pats' $ conValues v)
      VPInt i | VInt i == v -> (True, acc)
      VPBool b | VBool b == v -> (True, acc)
      VPChar c | VChar c == v -> (True, acc)
      VPString s | VString s == v -> (True, acc)
      VPTuple _ -> undefined
      VPList _ -> undefined
      _             -> (False, acc)



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
