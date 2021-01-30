{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API (runAPI, runStatic) where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Text hiding (filter)
import Lib.Markdown
import Lib.Post
import Network.Wai.Handler.Warp
import Servant hiding (Post)
import System.FilePath

type PostsAPI =
  "posts" :> QueryParam "tag" Text :> Get '[JSON] [Post]
    :<|> "posts" :> Capture "slug" Text :> Get '[JSON] Post

server :: FilePath -> Server PostsAPI
server dir = posts :<|> postWithKey
  where
    posts :: Maybe Text -> Handler [Post]
    posts Nothing = liftIO posts'
    posts (Just t) = liftIO $ posts' >>= (return . withTag t)

    postWithKey :: Text -> Handler Post
    postWithKey s = do
      post <- liftIO $ posts' >>= (return . withSlug s)
      case post of
        Nothing -> throwError err404
        Just p -> return p

    posts' =
      newestFirst
        . filter (not . fromMaybe False . draft)
        . catMaybes
        <$> mdDirToPosts (joinPath [dir, "posts"])

runAPI :: FilePath -> Int -> IO ()
runAPI dir port = run port serverApp
  where
    postsAPI :: Proxy PostsAPI
    postsAPI = Proxy

    serverApp :: Application
    serverApp = serve postsAPI $ server dir

type StaticAPI = Raw

runStatic :: FilePath -> Int -> IO ()
runStatic dir port = run port serverApp
  where
    staticAPI :: Proxy StaticAPI
    staticAPI = Proxy

    serverApp :: Application
    serverApp = serve staticAPI $ serveDirectoryFileServer dir
