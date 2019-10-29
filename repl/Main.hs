{-|
Description : hrafnar-lang repl
Module      : Main
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Hrafnar.Annotation
import           Hrafnar.AST
import           Hrafnar.Builtin
import           Hrafnar.Core
import           Hrafnar.Exception
import           Hrafnar.Inferer
import           Hrafnar.Parser
import           Hrafnar.Types

import           Control.Lens               hiding (setting)

import           Control.Monad.RWS
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Lazy
import           Data.Functor
import qualified Data.Graph                 as GR
import qualified Data.List                  as L
import qualified Data.Map.Strict            as MA
import qualified Data.Set                   as SE
import           System.Console.Haskeline
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- | Value and type environment in the repl.
data Env = Env
  { _valEnv   :: VEnv
  , _typeEnv  :: TEnv
  , _freshVar :: Int
  } deriving (Show)

makeLenses  ''Env

-- | State of the repl
newtype ReplState = ReplState
  { _env   :: Env
  }

makeLenses  ''ReplState

-- | Input monad with Env.
type Interpret a = InputT (StateT ReplState IO) a

-- | Block input.
type BlockInput a = InputT (WriterT String IO) a

-- | For REPL.
data Line
  = ExprLine Expr
  | DeclLine Decl
  | Command Command
  deriving (Eq, Show)

-- | Commands.
data Command
  = StartBlock
  | EndBlock
  | ShowType
  | ShowEnv
  deriving (Eq, Show)

-- | Command parser.
cmdParser :: Parser Command
cmdParser = char ':' *>
            ( string "{" $> StartBlock <|>
              string "}" $> EndBlock <|>
              string "t" $> ShowType <|>
              string "e" $> ShowEnv
            )

-- | Line Parser.
lineParser :: Parser Line
lineParser = try (DeclLine <$> declParser <* eof) <|>
             try (ExprLine <$> exprParser <* eof) <|>
             Command <$> cmdParser <* eof



-- | Parse a line and interpret it.
interpret :: String -> Interpret ()
interpret i = do
  liftIO $ parseTest lineParser i
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
      Command StartBlock -> do
        block <- liftIO . execWriterT $ runInputT blockSetting (withInterrupt blockLoop)
        catches (interpretBlock block)
               [ Handler inferError
               , Handler evalError
               ]
      Command ShowEnv ->
        lift (use env) >>= outputStrLn . show
      Command c ->
        outputStrLn $ show c
    Left e  -> outputStrLn $ show e

-- | Loop of block input
blockLoop :: BlockInput ()
blockLoop = getInputLine "| " >>= \case
    Nothing -> pure ()
    Just "" -> blockLoop
    Just i -> case parse (Left . Command <$> cmdParser <|> Right <$> getInput) "<repl>" i of
      Right (Right str) ->
        lift (tell $ str <> "\n") >> blockLoop
      Right (Left (Command EndBlock)) ->
        pure ()
      Right (Left (Command cmd)) ->
        outputStrLn (show cmd) >> blockLoop
      Left e ->
        error $ show e


-- | Evaluate an expression and show it.
interpretExpr :: Expr -> Interpret ()
interpretExpr e  = do
  env <- lift $ use env
  sc <- lift $ infer (env ^. typeEnv) e
  (v, t) <- lift $ evalRWST (evalExpr e) defaultSituation (initialState & #venv .~ env ^. valEnv)
  outputStrLn $ "Value: " <> show v
  outputStrLn $ "Type: " <> show sc
  outputStrLn "Effect: "
  outputStrLn . show . view _1 $ appEndo t (defaultSituation ^. #context, [])

-- | Interpret a declaration and add it to the environment.
interpretDecl :: Decl -> Interpret ()

interpretDecl (At _ TypeAnno{}) = outputStrLn "unavailable to declare type only"

interpretDecl (At _ (ExprDecl n e)) = do
  en <- lift $ use env
  sc <- lift $ infer (en ^. typeEnv) (withDummy $ Let [withDummy $ ExprDecl n e] (withDummy $ Var n))
  (v,_) <- lift $ evalRWST (compile e) defaultSituation (initialState & #venv .~ en ^. valEnv)
  lift $ env.valEnv .= MA.insert n v (en ^. valEnv)
  lift $ env.typeEnv .= MA.insert n sc (en ^. typeEnv)

interpretDecl (At _ (DataDecl n ns cons)) = do
  en <- lift $ use env
  ts <- lift $ dataDeclsToEnv [(n, (ns, cons))]
  let ds = MA.fromList $ compileData (n, (ns, cons))
  lift $ env.valEnv .= MA.union ds (en ^. valEnv)
  lift $ env.typeEnv .= MA.union ts (en ^. typeEnv)

-- | Interpret a block of statements.
interpretBlock :: String -> Interpret ()
interpretBlock src = do
  liftIO $ parseTest topLevel src
  case parse topLevel "<repl>" src of
    Right decls -> do
      (binds, dataDecls) <- lift $ scanDecls decls

      -- declare data constructors
      en <- lift $ use env
      dts <- lift $ dataDeclsToEnv dataDecls
      let ds = MA.unions $ fmap (MA.fromList . compileData) dataDecls
      lift $ env.valEnv .= MA.union ds (en ^. valEnv)
      lift $ env.typeEnv .= MA.union dts (en ^. typeEnv)

      -- declare expressions
      g <- lift $ sequence [SE.toList <$> fvsExpr e >>= (\fs -> pure ((n, (e, t)), n, fs)) | (n, (e, t)) <- binds]
      let iss = fmap GR.flattenSCC . GR.stronglyConnComp $ g
      go iss
        where
          go :: [[(Name, (Expr, Maybe Type))]] -> Interpret ()
          go [] = pure ()
          go (bs:bss) = do
            en <- lift $ use env
            freshEnv <- MA.fromList <$> traverse (\(n, _) -> (fresh <&> generalize) <&> (n,)) bs
            ((tenv, venv), _) <- lift $ evalRWST
              (do
                  (answers, vs) <- L.unzip <$> forM bs (\(n, (e, t)) -> do
                    (t', cs) <- inferExpr e
                    (v,_) <- lift $ evalRWST (compile e) defaultSituation (initialState & #venv .~ en ^. valEnv)
                    case t of
                      Just t'' -> pure ((t', EqConst t' t'': cs), (n, v))
                      Nothing  -> pure ((t', cs), (n, v))
                      )
                  su <- solve (mconcat $ fmap (^. _2) answers)
                  pure ( MA.fromList $ zipWith (\b s -> (b ^. _1, s)) bs $ fmap (\(t, _) -> generalize (apply su t)) answers
                       , MA.fromList vs
                       )

              ) (MA.union (en ^. typeEnv) freshEnv) (en ^. freshVar)
            lift $ env.typeEnv .= MA.union tenv (en ^. typeEnv)
            lift $ env.valEnv .= MA.union venv (en ^. valEnv)
            go bss


    Left e ->
      outputStrLn $ show e

-- | Return a fresh type value.
fresh :: Interpret Type
fresh = do
  n <- lift . use $ env.freshVar
  lift $ env.freshVar += 1
  pure . TyVar . TV $ show n

-- | Errors for type infering.
inferError :: InferenceException -> Interpret ()
inferError = \case
  TypeVariableNotFound v ->
    outputStrLn v
  e ->
    outputStrLn $ show e

-- | Errors for evaluation.
evalError :: EvalException -> Interpret ()
evalError = \case
  VariableNotDeclared v->
    outputStrLn $ "variable not declare: " <> v

-- | Loop interpreting.
loop :: Interpret ()
loop = handle (\Interrupt -> loop) $
  getInputLine "> " >>= \case
    Nothing -> pure ()
    Just "" -> loop
    Just i -> interpret i >> loop

-- | For completion.
search :: String -> [Completion]
search str = simpleCompletion <$> filter (L.isPrefixOf str)
             [
             ]

-- | Setting for repl.
setting :: Settings (StateT ReplState IO)
setting = Settings { historyFile = Nothing
                   , complete = completeWord Nothing " \t" $ pure . search
                   , autoAddHistory = True
                   }

-- | Setting for code block.
blockSetting :: Settings (WriterT String IO)
blockSetting = Settings { historyFile = Nothing
                   , complete = completeWord Nothing " \t" $ pure . search
                   , autoAddHistory = True
                   }

-- | Start repl.
main :: IO ()
main =
   evalStateT
   (runInputT setting (withInterrupt loop)) $
   ReplState (Env (fmap (view _1) builtins) (view _2 <$> builtins) 0)
