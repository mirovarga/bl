{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

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
  -- /posts?tag=&standalone=
  "posts"
    :> QueryParam "tag" Text
    :> QueryParam "standalone" (Filter 'Standalone)
    :> Get '[JSON] [Post]
    -- /posts/{index}
    :<|> "posts"
    :> Capture "index" Int
    :> Get '[JSON] Post
    -- /posts/{key}
    :<|> "posts"
    :> Capture "key" Text
    :> Get '[JSON] Post

data FilterParam = Standalone

data Filter (p :: FilterParam) = Yes | No | Both

instance FromHttpApiData (Filter p) where
  parseQueryParam (toLower -> "yes") = Right Yes
  parseQueryParam (toLower -> "no") = Right No
  parseQueryParam (toLower -> "both") = Right Both
  parseQueryParam value = Left $ "Unknown parameter value: " <> value

server :: FilePath -> Server PostsAPI
server dir = posts :<|> postWithIndex :<|> postWithKey
  where
    posts ::
      Maybe Text ->
      Maybe (Filter 'Standalone) ->
      Handler [Post]
    posts Nothing Nothing = liftIO posts'
    posts Nothing (Just No) = liftIO $ notStandalones <$> posts'
    posts Nothing (Just Yes) = liftIO $ standalones <$> posts'
    posts Nothing (Just Both) = liftIO posts'
    posts (Just t) _ = liftIO $ withTag t <$> posts'

    postWithIndex :: Int -> Handler Post
    postWithIndex i = do
      post <- liftIO $ lookup' i <$> posts'
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
      post <- liftIO $ withSlug s <$> posts'
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
