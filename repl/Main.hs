{-|
Description : hrafnar-lang repl
Module      : Main
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}
module Main where

import           Hrafnar.Annotation
import           Hrafnar.AST
import           Hrafnar.Builtin
import           Hrafnar.Core
import           Hrafnar.Exception
import           Hrafnar.Inferer
import           Hrafnar.Parser

import           Control.Lens               hiding (setting)

import           Control.Monad.RWS
import           Control.Monad.State.Strict
import qualified Data.List                  as L
import qualified Data.Map.Strict            as MA
import           System.Console.Haskeline
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- | Value and type nvironment in the repl.
data Env = Env
  { valEnv  :: VEnv
  , typeEnv :: TEnv
  }
-- | Input monad with Env.
type Interpret = InputT (StateT Env IO) ()

-- | For REPL.
data Line
  = ExprLine Expr
  | DeclLine Decl
  deriving (Eq, Show)

lineParser :: Parser Line
lineParser = try (ExprLine <$> exprParser) <|>
             DeclLine <$> declParser <*
             many newline <* eof

-- | For completion.
search :: String -> [Completion]
search str = simpleCompletion <$> filter (L.isPrefixOf str)
             [
             ]

-- | Setting for repl.
setting :: Settings (StateT Env IO)
setting = Settings { historyFile = Nothing
                   , complete = completeWord Nothing " \t" $ pure . search
                   , autoAddHistory = True
                   }

-- | Parse a line and interpret it.
interpret :: String -> Interpret
interpret i =
  case parse lineParser "<repl>" i of
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
    Left _  -> outputStrLn "something wrong"

-- | Evaluate an expression and show it.
interpretExpr :: Expr -> Interpret
interpretExpr e  = do
  env <- lift get
  _ <- lift $  infer (typeEnv env) e
  (v, t) <- lift $ evalRWST (evalExpr e) defaultSituation (initialState & #venv .~ valEnv env)
  outputStrLn $ "Value: " <> show v
  outputStrLn "Effect: "
  outputStrLn . show . view _1 $ appEndo t (defaultSituation ^. #context, [])

-- | Interpret a declaration and add it to the environment.
interpretDecl :: Decl -> Interpret

interpretDecl (At _ TypeAnno{}) = outputStrLn "unavailable to declare type only"

interpretDecl (At _ (ExprDecl n e)) = do
  env <- lift get
  sc <- lift $ infer (typeEnv env) (withDummy $ Let [withDummy $ ExprDecl n e] (withDummy $ Var n))
  (v,_) <- lift $ evalRWST (compile e) defaultSituation (initialState & #venv .~ valEnv env)
  lift . modify $ \Env{..} ->
    Env (MA.insert n v valEnv) (MA.insert n sc typeEnv)

interpretDecl (At _ (DataDecl n ns cons)) = do
  ts <- lift $ dataDeclsToEnv [(n, (ns, cons))]
  let ds = MA.fromList $ compileData (n, (ns, cons))
  lift . modify $ \Env{..} ->
    Env (MA.union valEnv ds) (MA.union typeEnv ts)

-- | Errors for type infering.
inferError :: InferenceException -> Interpret
inferError = \case
  TypeVariableNotFound v ->
    outputStrLn v
  e ->
    outputStrLn $ show e

-- | Errors for evaluation.
evalError :: EvalException -> Interpret
evalError = \case
  VariableNotDeclared v->
    outputStrLn $ "variable not declare: " <> v

-- | Loop interpreting.
loop :: Interpret
loop = handle (\Interrupt -> loop) $
  getInputLine "> " >>= \case
    Nothing -> pure ()
    Just "" -> loop
    Just i -> interpret i >> loop

-- | Start repl.
main :: IO ()
main =
   evalStateT
   (runInputT setting (withInterrupt loop)) $
   Env (fmap (view _1) builtins) (view _2 <$> builtins)
