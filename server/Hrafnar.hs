{-|
Module      : Hrafnar
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}

{-# LANGUAGE TypeApplications #-}

module Hrafnar
  ( takeOff
  , module Hrafnar.DI
  ) where

import           Hrafnar.Controller
import           Hrafnar.DI
import           Hrafnar.Hatch
import           Hrafnar.WebApi


import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Lens
import           Data.Extensible


-- | Let Hrafnar take off.
takeOff :: UseDI => IO ()
takeOff = do
  chan <- newLogChan
  _ <- async $ broadcastLog useLogger chan
  _ <- async $ relayLog stdoutLogger chan LogDebug
  logSettings
  _ <- async controller
  _ <- async hatch
  _ <- async runWebApi
  clock

-- | Logs Hrafnar-settings with level DEBUG
logSettings :: UseDI => IO ()
logSettings = hfoldMapWithIndexFor c f env
  where
    c = Proxy @ (KeyValue KnownSymbol Show)
    f h (Field (Identity x)) = debug useLogger $ stringAssocKey h <> ": " <> show x

-- | Generate tick @Event@.
clock :: (UseDI, UseEventChan) => IO ()
clock = do
  threadDelay . fromIntegral $ env ^. itemAssoc (Proxy @ "HRAF_CYCLE_MILLI_SECONDS") * 1000
  clock
