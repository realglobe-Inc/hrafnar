{-|
Module      : Hrafnar.DI.Resources
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}
module Hrafnar.DI.Resources
  (
    -- * Event channel
    UseEventChan
  , eventChan
    -- * TX channel
  , TxID(..)
  , tx
  -- * HTTPS client manager
  , httpsManager
  -- * Bundled resources
  , UseResources
  , recipeManager
  , recipeQueue
  , manageRecipe
  , injectResources
  , RecipeManagement(..)
  ) where

import           Hrafnar.Recipe.Event

import           Control.Concurrent.Async (Async, cancel)
import           Control.Concurrent.STM
import qualified Data.ByteString          as SBS
import qualified Data.Map                 as MA
import           Data.Reflection          (Given, give, given)
import           Data.ULID                (ULID, getULID)
import           Network.HTTP.Client      (Manager, newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)

-- | Type synonym.
type UseEventChan = Given (TChan Event)

-- | Get @Queue@ @Event@.
eventChan :: UseEventChan => TChan Event
eventChan = given

-- | Wrapped @ULID@ for @Odin@ queue.
newtype TxID = TxID { unTxID :: ULID } deriving(Eq, Ord, Show)


-- | Table of @Hrafnar@ queues.
type TxTable = TVar (MA.Map TxID (TQueue ()))

tx :: Given TxTable => TxTable
tx = given

-- | Get @Manager@.
httpsManager :: Given Manager => Manager
httpsManager = given

-- temporarily specify Int as key type
type RecipeManager = TVar (MA.Map ULID (Async ()))

recipeManager :: Given RecipeManager => RecipeManager
recipeManager = given

data RecipeManagement = RecipeAdd SBS.ByteString | RecipeRemove ULID

-- | temporary take ByteString
type RecipeQueue = TQueue RecipeManagement

manageRecipe :: UseResources => (SBS.ByteString -> IO (Async ())) -> IO ()
manageRecipe asyncAction = do
  management <- atomically $ readTQueue recipeQueue
  manager <- readTVarIO recipeManager
  case management of
    RecipeAdd recipeString -> do
      ulid <- getULID
      a <- asyncAction recipeString
      atomically $ writeTVar recipeManager (MA.insert ulid a manager)
    RecipeRemove i -> do
        cancel (manager MA.! i)
        atomically $ writeTVar recipeManager (MA.delete i manager)


recipeQueue :: Given RecipeQueue => RecipeQueue
recipeQueue = given

-- | Bundle resources.
type UseResources =
  ( Given TxTable
  , Given Manager
  , Given (TChan Event)
  , Given RecipeManager
  , Given RecipeQueue
  )

-- | Inject resouces.
injectResources :: (UseResources => a) -> IO a
injectResources io = do
  bus <- atomically newBroadcastTChan :: IO (TChan Event)
  manager <- newManager tlsManagerSettings
  txTable <- newTVarIO MA.empty :: IO TxTable
  rm <- newTVarIO MA.empty :: IO RecipeManager
  rQueue <- newTQueueIO :: IO RecipeQueue
  return $
    give bus $
    give txTable $
    give rm $
    give rQueue $
    give manager io
