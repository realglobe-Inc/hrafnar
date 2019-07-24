{-|
Description : Web API for dashboard.
Module      : Hrafnar.WebApi
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}
{-# LANGUAGE TemplateHaskell #-}

module Hrafnar.WebApi
  ( runWebApi
  , app
  )
where

import           Hrafnar.DI
import           Hrafnar.Recipe

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Lens                hiding ((.=))
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy        as LBS
import           Data.Extensible
import           Data.FileEmbed
import qualified Data.List                   as L
import qualified Data.Map.Strict             as MA
import           GHC.Natural                 (naturalToInt)
import           Network.HTTP.Media          ((//), (/:))
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Path
import           Path.IO
import           Servant                     hiding (Handler (..))
import qualified Servant                     (Handler (..))
import           System.FilePath.Posix       (dropTrailingPathSeparator)

root :: (MonadIO m, MonadThrow m, UseDI) => m (Path Abs Dir)
root = do
  let dir = env ^. (itemAssoc (Proxy @ "HRAF_PROJECT_DIR"))
  case parseAbsDir dir of
           Right d -> pure $ d
           Left _ -> do
             d <- parseRelDir dir
             pwd <- liftIO getCurrentDir
             pure $ pwd </> d

project :: (MonadIO m, MonadThrow m, UseDI) => String -> m (Path Abs Dir)
project name = (</>) <$> root <*> parseRelDir name


-- GET /project
type GetProjectResponse = Record
  '[ "projects" >: [String]
   ]
getProject :: UseDI => Servant.Handler GetProjectResponse
getProject = do
  (dirs, _) <- listDirRel =<< root
  pure $ #projects @= L.sort (fmap (dropTrailingPathSeparator . toFilePath) dirs) <: nil

-- POST /project/:name
type PostProjectRequest = Record
  '[
   ]

type PostProjectResponse = Record
  '[ "name" >: String
   ]

postProject :: UseDI => String  -> Servant.Handler PostProjectResponse
postProject name = do
  project name >>= liftIO . createDirIfMissing False
  pure $ #name @= name <: nil

-- POST /project/:name/:sourcepath
type PostSourceRequest = Record
  '[ "source" >: String
   ]

type PostSourceResponse = Record
  '[ "parse" >: Bool
   , "typeCheck" >: Bool
   , "message" >: Maybe String
   ]

postSource :: UseDI => String -> [FilePath] -> PostSourceRequest -> Servant.Handler PostSourceResponse
postSource name paths body = do
  proj <- project name
  p <- doesDirExist proj
  unless p $ throw err404 { errBody = "project not found" }
  path' <- parseRelFile $ L.intercalate "/" paths
  let path = proj </> path'
  debug useLogger $ "make" <> show path
  liftIO . writeFile (toFilePath path) $ body ^. #source
  flip catches [parserError, infererError] $ do
    decls <- parse $ body ^. #source
    _ <- infer MA.empty $ withDummy . Let decls . withDummy $ Var "main"
    pure $ #parse @= True <: #typeCheck @= True <: #message @= Nothing <: nil
  where
    parserError = Handler $ \case
      StringException e _ ->
        pure $ #parse @= False <: #typeCheck @= False <: #message @= Just e <: nil
    infererError = Handler $ \case
      TypeVariableNotFound e ->
        pure $
        #parse @= True <: #typeCheck @= False <:
        #message @= Just ("type variable not found: " <> e)  <:
        nil

-- static files
-- index.html
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML LBS.ByteString where
  mimeRender _ bs = bs


indexHtml :: Servant.Handler LBS.ByteString
indexHtml = pure $ LBS.fromStrict "" -- $(embedFile "./dist/index.html")

-- main.js
data JS

instance Accept JS where
  contentType _ = "text" // "javascript" /: ("charset", "utf-8")

instance MimeRender JS LBS.ByteString where
  mimeRender _ bs = bs


mainJS :: Servant.Handler LBS.ByteString
mainJS = pure $ LBS.fromStrict "" -- $(embedFile "./dist/main.js")

-- bundle APIs and static files serving
-- | bundled API

type Api =
  "api" :>
  (
    "project" :>
    ( Get '[JSON] GetProjectResponse :<|>
      Capture "name" String :> Post '[JSON] PostProjectResponse :<|>
      Capture "name" String :> CaptureAll "source" FilePath :> ReqBody '[JSON] PostSourceRequest :> Post '[JSON] PostSourceResponse
    )
  ) :<|>
  Get '[HTML] LBS.ByteString :<|>
  "main.js" :> Get '[HTML] LBS.ByteString

server :: UseDI => Server Api
server = ( getProject :<|>
           postProject :<|>
           postSource
         ) :<|>
         indexHtml :<|>
         mainJS

app :: UseDI => Application
app = simpleCors $ serve (Proxy @ Api) server

runWebApi :: UseDI => IO ()
runWebApi = do
  let port = naturalToInt $ env ^. itemAssoc (Proxy @"HRAF_DASHBOARD_PORT")
      settings = setPort port defaultSettings
  runSettings settings app
