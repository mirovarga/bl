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
  -- /posts?tag=&standalone=&draft=
  "posts"
    :> QueryParam "tag" Text
    :> QueryParam "standalone" (Filter 'Standalone)
    :> QueryParam "draft" (Filter 'Draft)
    :> Get '[JSON] [Post]
    -- /posts/{index}
    :<|> "posts"
    :> Capture "index" Int
    :> Get '[JSON] Post
    -- /posts/{key}
    :<|> "posts"
    :> Capture "key" Text
    :> Get '[JSON] Post

data FilterParam = Standalone | Draft

data Filter (p :: FilterParam) = Yes | No

instance FromHttpApiData (Filter p) where
  parseQueryParam (toLower -> "yes") = Right Yes
  parseQueryParam (toLower -> "no") = Right No
  parseQueryParam value = Left $ "Unknown parameter value: " <> value

server :: FilePath -> Server PostsAPI
server dir = posts :<|> postWithIndex :<|> postWithKey
  where
    posts ::
      Maybe Text ->
      Maybe (Filter 'Standalone) ->
      Maybe (Filter 'Draft) ->
      Handler [Post]
    posts Nothing s d = liftIO $ filterBy standalone' s . filterBy draft' d <$> posts'
    posts (Just t) s d = liftIO $ withTag t . filterBy standalone' s . filterBy draft' d <$> posts'

    filterBy _ Nothing = id
    filterBy f (Just Yes) = filter f
    filterBy f (Just No) = filter (not . f)

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
        . catMaybes
        <$> mdDirToPosts (joinPath [dir, "posts"])

run :: FilePath -> Int -> IO ()
run dir port = W.run port serverApp
  where
    postsAPI :: Proxy PostsAPI
    postsAPI = Proxy

    serverApp :: Application
    serverApp = serve postsAPI $ server dir
