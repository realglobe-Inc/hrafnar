module Main where

import           Hrafnar.Annotation
import           Hrafnar.AST
import           Hrafnar.Builtin
import           Hrafnar.Core
import           Hrafnar.Exception
import           Hrafnar.Inferer
import           Hrafnar.Lexer
import           Hrafnar.Parser

import           Control.Lens               hiding (setting)

import           Control.Monad.RWS
import           Control.Monad.State.Strict
import qualified Data.List                  as L
import qualified Data.Map.Strict            as MA
import           System.Console.Haskeline


data Env = Env
  { valEnv  :: VEnv
  , typeEnv :: TEnv
  }

type Interpret = InputT (StateT Env IO) ()

search :: String -> [Completion]
search str = simpleCompletion <$> filter (L.isPrefixOf str)
             [
             ]

setting :: Settings (StateT Env IO)
setting = Settings { historyFile = Nothing
                   , complete = completeWord Nothing " \t" $ pure . search
                   , autoAddHistory = True
                   }
interpret :: String -> Interpret
interpret i =
  case runAlex i lineParser of
    Right r -> case r of
      ExprLine e ->
        catches (interpretExpr e)
               [ Handler inferError
               , Handler evalError
               ]
      DeclLine d ->
        catches (interpretDecl d)
               [ Handler inferError
               ]
    Left l  -> outputStrLn l

interpretExpr :: Expr -> Interpret
interpretExpr e  = do
  env <- lift get
  _ <- lift $  infer (typeEnv env) e
  (v, t) <- lift $ evalRWST (evalExpr e) defaultSituation (initialState & #venv .~ valEnv env)
  outputStrLn $ "Value: " <> show v
  outputStrLn "Effect: "
  outputStrLn . show . view _1 $ appEndo t (defaultSituation ^. #context, [])

interpretDecl :: Decl -> Interpret
interpretDecl TypeAnno{} = outputStrLn "unavailable to declare type only"
interpretDecl (ExprDecl n e) = do
  env <- lift get
  sc <- lift $ infer (typeEnv env) (withDummy $ Let [ExprDecl n e] (withDummy $ Var n))
  (v,_) <- lift $ evalRWST (compile e) defaultSituation (initialState & #venv .~ valEnv env)
  lift . modify $ \Env{..} ->
    Env (MA.insert n v valEnv) (MA.insert n sc typeEnv)

inferError :: InferenceException -> Interpret
inferError = \case
  TypeVariableNotFound v ->
    outputStrLn v

evalError :: EvalException -> Interpret
evalError = \case
  VariableNotDeclared v ->
    outputStrLn v

loop :: Interpret
loop = handle (\Interrupt -> loop) $
  getInputLine "> " >>= \case
    Nothing -> pure ()
    Just "" -> loop
    Just i -> interpret i >> loop

main :: IO ()
main =
   evalStateT
   (runInputT setting (withInterrupt loop)) $
   Env (fmap (view _1) builtins) (view _2 <$> builtins)
