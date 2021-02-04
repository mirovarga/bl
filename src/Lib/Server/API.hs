{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Server.API (run) where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Text hiding (filter, length)
import Lib.Markdown
import Lib.Post
import qualified Network.Wai.Handler.Warp as W
import Servant hiding (Post)
import System.FilePath

type PostsAPI =
  "posts" :> QueryParam "tag" Text :> Get '[JSON] [Post]
    :<|> "posts" :> Capture "index" Int :> Get '[JSON] Post
    :<|> "posts" :> Capture "slug" Text :> Get '[JSON] Post

server :: FilePath -> Server PostsAPI
server dir = posts :<|> postWithIndex :<|> postWithKey
  where
    posts :: Maybe Text -> Handler [Post]
    posts Nothing = liftIO posts'
    posts (Just t) = liftIO $ posts' >>= (return . withTag t)

    postWithIndex :: Int -> Handler Post
    postWithIndex i = do
      post <- liftIO $ posts' >>= (return . lookup' i)
      case post of
        Nothing -> throwError err404
        (Just p) -> return p
      where
        lookup' :: Int -> [Post] -> Maybe Post
        lookup' index posts
          | index >= 0 && index < length posts = Just (posts !! index)
          | otherwise = Nothing

    postWithKey :: Text -> Handler Post
    postWithKey s = do
      post <- liftIO $ posts' >>= (return . withSlug s)
      case post of
        Nothing -> throwError err404
        Just p -> return p

    posts' :: IO [Post]
    posts' =
      newestFirst
        . filter (not . fromMaybe False . draft)
        . catMaybes
        <$> mdDirToPosts (joinPath [dir, "posts"])

run :: FilePath -> Int -> IO ()
run dir port = W.run port serverApp
  where
    postsAPI :: Proxy PostsAPI
    postsAPI = Proxy

    serverApp :: Application
    serverApp = serve postsAPI $ server dir
