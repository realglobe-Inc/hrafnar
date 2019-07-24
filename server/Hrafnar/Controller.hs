{- |
Description : Control events.
Module      : Hrafnar.Controller
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  :  REALGLOBE INC.
-}

module Hrafnar.Controller
  ( controller
  ) where

import           Hrafnar.DI
import           Hrafnar.Recipe

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception.Safe
import           Control.Lens             hiding (Const, Context, List)
import           Control.Monad
import           Data.Extensible
import qualified Data.Text                as ST

-- NOTE: import temporary
import qualified Data.Map                 as MA

-- | Main controller.
controller :: UseDI => IO ()
controller = do
  _ <- async listenEvent
  _ <- async listenRecipe
  --cook slackEchoRecipe
  pure ()

listenEvent :: UseDI => IO ()
listenEvent = do
  chan <- atomically $ dupTChan eventChan
  forever $ atomically (readTChan chan) >>= debug useLogger . ST.pack . show

-- | recv recipeQueue and then run recipe
listenRecipe :: UseDI => IO ()
listenRecipe = undefined
{-  forever $ manageRecipe $ \s -> async $ case parse . ST.unpack . TE.decodeUtf8 $ s of
  Right r -> cook $ #initial @= MA.empty <: #cookbook @= r <: nil
  Left e  -> do
    err useLogger e
    throwString e
-}

-- | Start cooking with @Recipe@.
cook :: UseDI => Recipe -> IO ()
cook recipe = do
  debug useLogger . ST.pack $ show recipe
  chan <- atomically $ dupTChan eventChan
  entry <- case lookup "main" $ recipe ^. #cookbook of
    Just v -> pure v
    _ -> do
      warn useLogger $: "\"main\" is not declared."
      throwString "main is not exist"
  void $ turn chan entry (recipe ^. #initial)

-- | Turn a step.
turn :: UseDI => TChan Event -> Expr -> ExprTable -> IO ExprTable
turn chan cookbook context =
  do
    event <- atomically (readTChan chan)
    let situation =
          #event @= event <:
          #context @= context <:
          #implant @= MA.empty <:
          #effects @= MA.empty <:
          nil :: Situation
    (newContext, effects) <- case update cookbook situation of
      Right r -> pure r
      Left e  -> do
        debug useLogger . ST.pack $ show e
        throw e
    debug useLogger . ST.pack $ show newContext
    newEvents <- runEffect $ sequence effects
    mapM_ (atomically . writeTChan chan) newEvents
    turn chan cookbook newContext

