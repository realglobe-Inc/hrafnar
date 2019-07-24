module Hrafnar.WebApiSpec where

import           Hrafnar.DI
import           Hrafnar.WebApi

import           Control.Exception.Safe
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString            as SBS
import           Data.ByteString.Lazy.Char8
import           Data.CaseInsensitive
import           Data.Extensible
import qualified Data.List                  as L
import           Data.String
import qualified Data.Vector                as V
import           Network.Wai
import           Network.Wai.Test
import           Path
import           Path.IO
import           Test.Hspec
import           Test.Hspec.Wai

obj :: IsString a => [Pair] -> a
obj = fromString . unpack . encode . object

post' :: SBS.ByteString -> [Pair] -> WaiSession SResponse
post' url = Test.Hspec.Wai.request "POST" url [(mk "Content-Type", "application/json")] . obj



withTemp :: (Application -> IO()) -> IO ()
withTemp action = do
  sysTemp <- getTempDir
  temp <- createTempDir sysTemp "hrafnar"
  res <- tryAny . join $ action <$> injectToIO (pure app) temp
  removeDirRecur temp
  case res of
    Right _ -> pure ()
    Left e  -> throw e

createProject :: SBS.ByteString -> WaiSession SResponse
createProject p = post' ("/api/project/" <> p) []

spec :: Spec
spec = around withTemp $ do

  describe "/project" $ do
    let endpoint = "/api/project"

    describe "get" $

      it "valid" $ do
        _ <- createProject "foo"
        _ <- createProject "bar"
        _ <- createProject "baz"
        get endpoint
          `shouldRespondWith`
          obj
          [ "projects" .= Array (V.fromList . fmap String $ L.sort
            [ "foo"
            , "bar"
            , "baz"
            ])
          ]

  describe "/project/:name" $ do
    let endpoint = "/api/project/example"

    describe "post" $ do

      it "valid" $ do
        post' endpoint
          []
          `shouldRespondWith`
          obj
          [ "name" .= String "example"
          ]


  describe "/project/:name/sourcefile" $ do
    let endpoint = "/api/project/example/source"

    describe "post" $ do

      it "valid" $ do
        _ <- createProject "example"
        post' endpoint
          [ "source" .= String "main = 123" ]
          `shouldRespondWith`
          obj
          [ "parse" .= Bool True
          , "typeCheck" .= Bool True
          , "message" .= Null
          ]

      it "fails parsing" $ do
        _ <- createProject "example"
        post' endpoint
          [ "source" .= String "nyaan" ]
          `shouldRespondWith`
          obj
          [ "parse" .= Bool False
          , "typeCheck" .= Bool False
          , "message" .= String "parseError: TkEof"
          ]


injectToIO :: (UseDI => IO a) -> Path Abs Dir -> IO a
injectToIO io temp = do
  let sysEnv =
        itemAssoc (Proxy @ "HRAF_DASHBOARD_PORT") @= 8080 <:
        itemAssoc (Proxy @ "HRAF_BUFFER_SIZE") @= 4096 <:
        itemAssoc (Proxy @ "HRAF_ODIN_HOST") @= "localhost" <:
        itemAssoc (Proxy @ "HRAF_ODIN_PORT") @= 9000 <:
        itemAssoc (Proxy @ "HRAF_CYCLE_MILLI_SECONDS") @= 1000 <:
        itemAssoc (Proxy @ "HRAF_CA_FILE") @= "./rootCA.pem" <:
        itemAssoc (Proxy @ "HRAF_PRODUCTION") @= False <:
        itemAssoc (Proxy @ "HRAF_PROJECT_DIR") @= toFilePath temp <:
        nil :: EnvType EnvFields
  lq <- newLogQueue
  join $ injectResources $
    give lq $
    give sysEnv io
