{-|
Description : UNIX domain server for debugging..
Module      : Hrafnar.Hatch
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.

-}
module Hrafnar.Hatch
  ( hatch
  ) where

import           Hrafnar.DI

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad
import qualified Data.Text              as ST
import qualified Data.Text.Encoding     as STE
import           Data.ULID              (ulidFromInteger)
import           GHC.IO.Handle
import           Network.Socket         hiding (recv)
import           System.Directory
import           System.IO

hatch :: UseDI => IO ()
hatch =
  bracket open close loop
  where
    open= do
      sock <- socket AF_UNIX Stream 0
      setSocketOption sock ReuseAddr 1
      setCloseOnExecIfNeeded $ fdSocket sock
      doesFileExist "./hrafnar.sock" >>= flip when (removeFile "./hrafnar.sock")
      bind sock (SockAddrUnix "./hrafnar.sock")
      listen sock 10
      return sock
    loop sock = forever $ do
      (sock', _) <- accept sock
      h <- socketToHandle sock' ReadWriteMode
      debug useLogger $: "Open the hatch!"
      void $ forkFinally
        (talk h)
        (\_ ->
            hClose h
        )
    talk h = do
      hGetLine h <&> words >>= \case
        [] ->
          hPutStr h "Nice chatting with you.\n" >> throwString "end."
        ["echo", s] ->
          debug useLogger s
        ["testEvent"] -> atomically . writeTChan eventChan $ #testEvent # ()
        ["noop"] -> atomically . writeTChan eventChan $ #noop # ()
        "addRecipe" : r -> atomically $ writeTQueue recipeQueue (RecipeAdd . STE.encodeUtf8 . ST.pack . join $ r)
        ["removeRecipe", rid] -> atomically $ writeTQueue recipeQueue (RecipeRemove . ulidFromInteger $ (read rid :: Integer))
        _ -> hPutStr h "I can't understand!\n"
      talk h

