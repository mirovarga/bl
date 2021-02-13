{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
  "posts"
    :> ( QueryParam "tag" Text :> QueryFlag "also-standalones" :> QueryFlag "only-standalones" :> Get '[JSON] [Post]
           :<|> Capture "index" Int :> Get '[JSON] Post
           :<|> Capture "key" Text :> Get '[JSON] Post
       )

server :: FilePath -> Server PostsAPI
server dir = posts :<|> postWithIndex :<|> postWithKey
  where
    posts :: Maybe Text -> Bool -> Bool -> Handler [Post]
    posts Nothing False False = liftIO $ notStandalones <$> posts'
    posts Nothing True False = liftIO posts'
    posts Nothing False True = liftIO $ standalones <$> posts'
    posts Nothing True True =
      throwError
        err400 {errBody = "Use either 'also-standalones' or 'only-standalones'."}
    posts (Just t) _ _ = liftIO $ withTag t <$> posts'

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
