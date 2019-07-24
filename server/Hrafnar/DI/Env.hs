{-|
Description : Environment values
Module      : Hrafnar.DI.Env
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}

{-# LANGUAGE TypeApplications #-}

module Hrafnar.DI.Env
  ( getEnv
  , env
  , UseEnv
  , EnvFields
  , EnvType
  ) where

import           Control.Applicative   ((<|>))
import           Control.Monad         (MonadPlus, mzero)
import           Data.Extensible
import           Data.Extensible.Field (RecordOf)
import           Data.Functor.Identity
import           Data.List             (lookup)
import           Data.Reflection       (Given, given)
import           Numeric.Natural       (Natural)
import           System.Environment    (getEnvironment)
import           Text.Read             (readMaybe)


-- | merge default env with environment variable
mergeEnv :: Forall (KeyValue KnownSymbol ReadMaybe) f => EnvMaybe f -> IO (EnvType f)
mergeEnv envDef = getEnvironment >>= maybeEnv . flip merge envDef

-- | Fail when lack env.
maybeEnv :: MonadPlus m => EnvMaybe f -> m (EnvType f)
maybeEnv = htraverse $
  \case
    Field (Just v) -> pure $ Field (Identity v)
    _ -> mzero
{-
-- | Look up and read environment value.
readEnv :: Read a => String -> EnvKeyValue -> Maybe a
readEnv k kvs = readMaybe =<< lookup k kvs
-}
type EnvKeyValue = [(String, String)]

-- | Environment configuration.
type EnvType f = Record f

-- | Environment configuration with nullable fields.
type EnvMaybe f =  RecordOf Maybe f

merge :: Forall (KeyValue KnownSymbol ReadMaybe) xs => EnvKeyValue -> EnvMaybe xs -> EnvMaybe xs
merge e =
  hmapWithIndexFor (Proxy @ (KeyValue KnownSymbol ReadMaybe)) f
  where
    f :: KeyValue KnownSymbol ReadMaybe x => Membership xs x -> Field Maybe x -> Field Maybe x
    f m (Field m') = Field $ readMaybe' (stringAssocKey m) e <|> m'

class Read a => ReadMaybe a where
  readMaybe' :: String -> EnvKeyValue -> Maybe a
  readMaybe' k kvs = readMaybe =<< lookup k kvs

instance ReadMaybe String where readMaybe' = lookup

instance ReadMaybe Natural

instance ReadMaybe Bool

getEnv :: IO (EnvType EnvFields)
getEnv = mergeEnv envDefault

-- | Env values default.
envDefault :: EnvMaybe EnvFields
envDefault =
  itemAssoc (Proxy @ "HRAF_DASHBOARD_PORT") @= Just 8080 <:
  itemAssoc (Proxy @ "HRAF_BUFFER_SIZE") @= Just 4096 <:
  itemAssoc (Proxy @ "HRAF_ODIN_HOST") @= Just "localhost" <:
  itemAssoc (Proxy @ "HRAF_ODIN_PORT") @= Just 9000 <:
  itemAssoc (Proxy @ "HRAF_CYCLE_MILLI_SECONDS") @= Just 1000 <:
  itemAssoc (Proxy @ "HRAF_CA_FILE") @= Just "./rootCA.pem" <:
  itemAssoc (Proxy @ "HRAF_PRODUCTION") @= Just False <:
  itemAssoc (Proxy @ "HRAF_PROJECT_DIR") @= Just "./hrafnar-projects" <:
  nil

-- | Definitions of environment values.
type EnvFields =
  '[ "HRAF_DASHBOARD_PORT" >: Natural
   , "HRAF_BUFFER_SIZE" >: Natural
   , "HRAF_ODIN_HOST" >: String
   , "HRAF_ODIN_PORT" >: Natural
   , "HRAF_CYCLE_MILLI_SECONDS" >: Natural
   , "HRAF_CA_FILE" >: String
   , "HRAF_PRODUCTION" >: Bool
   , "HRAF_PROJECT_DIR" >: String
   ]

-- | Type synonym.
type UseEnv = Given (EnvType EnvFields)

-- | Shorthand.
env :: UseEnv => EnvType EnvFields
env = given

