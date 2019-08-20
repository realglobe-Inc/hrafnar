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
import qualified Data.Text                  as ST
import qualified Data.Vector                as V
import           Network.Wai
import           Network.Wai.Test
import           Path
import           Path.IO                    hiding (createDir)
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

createDir :: SBS.ByteString -> SBS.ByteString -> WaiSession SResponse
createDir proj path =  post' ("/api/project/" <> proj <> "/" <> path) []

createFile :: SBS.ByteString -> SBS.ByteString -> ST.Text -> WaiSession SResponse
createFile proj path source =  post' ("/api/project/" <> proj <> "/" <> path) [ "source" .= String source ]

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
          [ "projects" .= Array
            (V.fromList . fmap (\name -> object ["name" .= String name]) $ L.sort
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


  describe "/project/:name/*path" $ do
    let endpoint = "/api/project/example"

    describe "get" $ do

      it "file list" $ do
        _ <- createProject "example"
        _ <- createDir "example" "hoge"
        _ <- createDir "example" "fuga"
        _ <- createFile "example" "foo" ""
        _ <- createFile "example" "bar" ""
        get endpoint
          `shouldRespondWith`
          obj
          [ "dirs" .= Array (V.fromList
            [ object [ "dir" .= Bool True, "name" .= String "fuga/" ]
            , object [ "dir" .= Bool True, "name" .= String "hoge/" ]
            , object [ "dir" .= Bool False, "name" .= String "bar" ]
            , object [ "dir" .= Bool False, "name" .= String "foo" ]
            ])
          ]

      it "file content" $ do
        _ <- createProject "example"
        _ <- createFile "example" "nyaan" "nyaan"
        get (endpoint <> "/nyaan")
          `shouldRespondWith`
          obj
          [ "file" .= object [ "name" .= String "nyaan", "content" .= String "nyaan" ]
          ]


    describe "post" $ do

      it "make dir" $ do
        _ <- createProject "example"
        post' (endpoint <> "/hoge")
          [ "source" .= Null ]
          `shouldRespondWith`
          obj
          [ "result" .= Null
          ]


      it "make not hl file" $ do
        _ <- createProject "example"
        post' (endpoint <> "/fuga")
          [ "source" .= String "lorem ipsum" ]
          `shouldRespondWith`
          obj
          [ "result" .= Null
          ]

      it "valid hl source" $ do
        _ <- createProject "example"
        post' (endpoint <> "/Main.hl")
          [ "source" .= String "main = 123" ]
          `shouldRespondWith`
          obj [ "result" .= object
                [ "parse" .= Bool True
                , "typeCheck" .= Bool True
                , "message" .= Null
                ]
              ]

      it "invalid hl source" $ do
        _ <- createProject "example"
        post' (endpoint <> "/Main.hl")
          [ "source" .= String "nyaan" ]
          `shouldRespondWith`
          obj [ "result" .= object
                [ "parse" .= Bool False
                , "typeCheck" .= Bool False
                , "message" .= String "parseError: TkEof"
                ]
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
