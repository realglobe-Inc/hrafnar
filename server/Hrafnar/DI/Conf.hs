{-|
Description : Manage configurations loaded dynamically.
Module      : Hrafnar.DI.Conf
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}
{-# LANGUAGE TypeApplications #-}
module Hrafnar.DI.Conf
  ( injectWebApiConf
  , webApi
  , WebApiConf
  , UseConf
  , Conf(..)
  , ConfSource(..)
  ) where

import           Control.Concurrent.STM
import           Data.Aeson             (FromJSON)
import qualified Data.Map.Strict        as MA
import           Data.Reflection
import qualified Data.Text              as ST
import           Data.Yaml              (decodeFileThrow)


-- | Readable formats.
type Readable a = FromJSON a

-- | Reloadable configuration.
class Conf a b where
  conf :: a b -> IO b
  reload :: a b -> IO ()

data SomeConf b = forall a. Conf a b => SomeConf (a b)

instance Readable b => Conf SomeConf b where
  conf (SomeConf i) =  conf i
  reload (SomeConf i) = reload i

type UseConf a = Given (SomeConf a)

data FromYaml b = FromYaml FilePath (TVar b)

instance Readable b => Conf FromYaml b where
  conf (FromYaml _ b) =  readTVarIO b
  reload (FromYaml p b) = decodeFileThrow p >>= atomically . writeTVar b

data ConfSource
  = Yaml FilePath
  | Csv FilePath
  | Empty
  deriving(Eq, Show, Ord)

newConfFrom :: forall a.Readable a => ConfSource -> IO (SomeConf a)
newConfFrom = \case
  Yaml path -> do
    t <- newTVarIO =<< (decodeFileThrow path :: IO a)
    return $ SomeConf (FromYaml path t)
  Csv path -> do
    print path
    return undefined
  Empty ->
    return undefined

-- | WebAPI definition.
type WebApiConf = MA.Map ST.Text ST.Text -- TODO: temporary setting definition

webApi :: UseConf WebApiConf => SomeConf WebApiConf
webApi = given

injectWebApiConf :: ConfSource ->  (UseConf WebApiConf => a) -> IO a
injectWebApiConf src io = do
  newConf <- newConfFrom @WebApiConf src
  return $ give newConf io
