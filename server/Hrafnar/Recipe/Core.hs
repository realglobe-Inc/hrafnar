{-|
Description : Definition of Recipe
Module      : Hrafnar.Recipe.Core
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}

module Hrafnar.Recipe.Core
  ( Recipe
  , Effect(..)
  , ValTable
  , ExprTable
  , Situation
  , LocalState
  , Transaction
  , initialState
  , Eval
  , Value(..)
  , VEnv
  , compile
  , evalMain
  , evalExpr
  , link
  , update
  , combI
  , module Hrafnar.Recipe.AST
  , (!)
  ) where

import           Hrafnar.Recipe.Annotation
import           Hrafnar.Recipe.AST
import           Hrafnar.Recipe.Event
import           Hrafnar.Recipe.Exception
import           Hrafnar.Recipe.Types
import           Hrafnar.Recipe.Value

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Lens              hiding (Const, Context, List)
import           Control.Monad.RWS
import           Data.Extensible           hiding (State)
import qualified Data.Map                  as MA
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
          letInBind (ExprDecl n e':ds') expr' =
            liftA2 VApp (abstract n <$> (letInBind ds' expr')) (compile e')
          letInBind (TypeAnno{}:ds') expr' = letInBind ds' expr'
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
      PVar n       -> pure $ VPVar n
      PLit (Int i) -> pure $ VPInt i
      PCon n pats  -> VPCon n <$> traverse compilePat' pats



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
consumePats value ((pat, patv):xs) = case pat of
  VPVar n                   -> link (VApp (abstract n patv) value)
  VPInt i | VInt i == value -> link patv
  VPWildcard                -> link patv
  _                         -> consumePats value xs



evalExpr :: Expr -> Eval Value
evalExpr expr = compile expr >>= link

-- | compile all decls and link main
evalMain :: [Decl] -> Eval Value
evalMain decls = do
  values <- MA.fromList <$> traverse (\(n, e) -> compile e >>= pure . (n,)) (extractExprs decls)
  vars <- gets $ view #venv
  let dataDecls = extractData decls
      dataEnv = MA.fromList . join $ fmap compileData dataDecls
  modify $ #venv .~ MA.unions [vars, values, dataEnv]

  case MA.lookup "main" values of
    Just v  -> link v
    Nothing -> throw $ VariableNotDeclared "main"


update :: MonadThrow m => Expr -> Situation -> m (ExprTable, [Effect Event])
update cookbook situation = do
  (_, transaction) <- evalRWST (evalExpr cookbook) situation initialState
  pure $ appEndo transaction (situation ^. #context, [])
