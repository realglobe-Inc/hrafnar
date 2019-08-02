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
import qualified Data.Aeson                  as AE
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
type Project = Record
  '[ "name" >: String
   ]

type GetProjectResponse = Record
  '[ "projects" >: [Project]
   ]

getProject :: UseDI => Servant.Handler GetProjectResponse
getProject = do
  (dirs, _) <- listDirRel =<< root
  let projects = L.sort
                 (fmap (\n ->
                          #name @= (dropTrailingPathSeparator $ toFilePath n) <:
                          nil) dirs)
  pure $ #projects @= projects <: nil

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

-- GET /project/:name/*path

type DirInfo = Record
  '[ "name" >: String
   , "dir" >: Bool
   ]

type FileContent = Record
  '[ "name" >: String
   , "content" >: String
   ]

type GetDirsOrFileResponse = Variant
  '[ "dirs" >: [DirInfo]
   , "file" >: FileContent
   ]

instance AE.ToJSON GetDirsOrFileResponse where
  toJSON = matchField $
    #dirs @= (\d -> AE.object ["dirs" AE..= d]) <:
    #file @= (\f -> AE.object ["file" AE..= f]) <:
    nil

getDirsOrFile :: UseDI => String -> [FilePath] -> Servant.Handler GetDirsOrFileResponse
getDirsOrFile proj path = do
  p <- liftA2 (</>) root (parseRelDir $ L.intercalate "/" (path <> [proj]))
  isDir <- doesDirExist p
  if isDir
    then do
    (ds, fs) <- listDirRel p
    let dirs = fmap (\d -> #name @= toFilePath d <: #dir @= True <: nil) ds <>
               fmap (\f -> #name @= toFilePath f <: #dir @= False <: nil) fs
    pure $ #dirs # dirs

    else do
    let name = last path
    content <- liftIO . readFile $ toFilePath p
    pure $ #file # (#name @= name <: #content @= content <: nil)


-- POST /project/:name/*path
type PostDirOrSourceRequest = Record
  '[ "source" >: Maybe String
   ]

type PostDirOrSourceResponse = Record
  '[ "result" >: Maybe FileResponse
   ]

type FileResponse = Record
  '[ "parse" >: Bool
   , "typeCheck" >: Bool
   , "message" >: Maybe String
   ]

postDirOrSource :: UseDI => String -> [FilePath] -> PostDirOrSourceRequest -> Servant.Handler PostDirOrSourceResponse
postDirOrSource name paths body = do
  proj <- project name
  p <- doesDirExist proj
  unless p $ throw err404 { errBody = "project not found" }
  case body ^. #source of
    Just source -> do
      path' <- parseRelFile $ L.intercalate "/" paths
      let path = proj </> path'
      case fileExtension path of
        ".hl" -> do
          debug useLogger $ "make hl source " <> show path
          liftIO $ writeFile (toFilePath path) source
          flip catches [parserError, infererError] $ do
            decls <- parse source
            _ <- infer MA.empty $ withDummy . Let decls . withDummy $ Var "main"
            pure $ #result @= Just (#parse @= True <: #typeCheck @= True <: #message @= Nothing <: nil) <: nil
        _ -> do
          debug useLogger $ "make file " <> show path
          liftIO $ writeFile (toFilePath path) source
          pure $ #result @= Nothing <: nil
    Nothing -> do
      path' <- parseRelDir $ L.intercalate "/" paths
      let path = proj </> path'
      debug useLogger $ "make dir " <> show path
      createDir path
      pure $ #result @= Nothing <: nil
  where
    parserError = Handler $ \case
      StringException e _ ->
        pure $ #result @= Just (#parse @= False <: #typeCheck @= False <: #message @= Just e <: nil) <: nil
    infererError = Handler $ \case
      TypeVariableNotFound e ->
        pure $ #result @= Just
        ( #parse @= True <: #typeCheck @= False <:
          #message @= Just ("type variable not found: " <> e)  <:
          nil
        ) <: nil

-- static files
-- index.html
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML LBS.ByteString where
  mimeRender _ bs = bs


indexHtml :: Servant.Handler LBS.ByteString
indexHtml = pure $ LBS.fromStrict $(embedFile "./dist/index.html")

-- main.js
data JS

instance Accept JS where
  contentType _ = "text" // "javascript" /: ("charset", "utf-8")

instance MimeRender JS LBS.ByteString where
  mimeRender _ bs = bs


mainJS :: Servant.Handler LBS.ByteString
mainJS = pure $ LBS.fromStrict $(embedFile "./dist/main.js")

-- bundle APIs and static files serving
-- | bundled API

type Api =
  "api" :>
  (
    "project" :>
    ( Get '[JSON] GetProjectResponse :<|>
      Capture "name" String :> Post '[JSON] PostProjectResponse :<|>
      Capture "name" String :> CaptureAll "path" FilePath :> Get '[JSON] GetDirsOrFileResponse :<|>
      Capture "name" String :> CaptureAll "source" FilePath :> ReqBody '[JSON] PostDirOrSourceRequest :> Post '[JSON] PostDirOrSourceResponse
    )
  ) :<|>
  Get '[HTML] LBS.ByteString :<|>
  "main.js" :> Get '[HTML] LBS.ByteString

server :: UseDI => Server Api
server = ( getProject :<|>
           postProject :<|>
           getDirsOrFile :<|>
           postDirOrSource
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
