{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module API (run) where

import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Data.Ord
import Data.Text hiding (filter, length)
import Markdown
import qualified Network.Wai.Handler.Warp as W
import Post
import Servant hiding (Post)
import System.FilePath

type PostsAPI =
  -- /posts?tags=&standalone=&draft=&sort=
  "posts"
    :> QueryParam "tags" Text
    :> QueryParam "standalone" (Filter 'Standalone)
    :> QueryParam "draft" (Filter 'Draft)
    :> QueryParam "sort" Sort
    :> Get '[JSON] [Post]
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

data SortDirection = Asc | Desc

data Sort
  = Title SortDirection
  | Created SortDirection
  | Key SortDirection

instance FromHttpApiData Sort where
  parseQueryParam (toLower -> "title") = Right $ Title Asc
  parseQueryParam (toLower -> "-title") = Right $ Title Desc
  parseQueryParam (toLower -> "created") = Right $ Created Asc
  parseQueryParam (toLower -> "-created") = Right $ Created Desc
  parseQueryParam (toLower -> "key") = Right $ Key Asc
  parseQueryParam (toLower -> "-key") = Right $ Key Desc
  parseQueryParam value = Left $ "Unknown parameter value: " <> value

server :: FilePath -> Server PostsAPI
server dir = posts :<|> postWithKey
  where
    posts ::
      Maybe Text ->
      Maybe (Filter 'Standalone) ->
      Maybe (Filter 'Draft) ->
      Maybe Sort ->
      Handler [Post]
    posts Nothing s d s' =
      liftIO $
        sortBy' s'
          . filterBy standalone' s
          . filterBy draft' d
          <$> posts'
    posts (Just ts) s d s' =
      liftIO $
        sortBy' s'
          . withTags (splitOn "," ts)
          . filterBy standalone' s
          . filterBy draft' d
          <$> posts'

    filterBy _ Nothing = id
    filterBy f (Just Yes) = filter f
    filterBy f (Just No) = filter (not . f)

    sortBy' Nothing = id
    sortBy' (Just (Title Asc)) = sortOn title
    sortBy' (Just (Title Desc)) = sortOn (Down . title)
    sortBy' (Just (Created Asc)) = sortOn created
    sortBy' (Just (Created Desc)) = sortOn (Down . created)
    sortBy' (Just (Key Asc)) = sortOn key
    sortBy' (Just (Key Desc)) = sortOn (Down . key)

    postWithKey :: Text -> Handler Post
    postWithKey s = do
      post <- liftIO $ withSlug s <$> posts'
      case post of
        Nothing -> throwError err404
        Just p -> return p

    posts' :: IO [Post]
    posts' = catMaybes <$> mdDirToPosts (joinPath [dir, "posts"])

run :: FilePath -> Int -> IO ()
run dir port = do
  W.run port serverApp
  where
    postsAPI :: Proxy PostsAPI
    postsAPI = Proxy

    serverApp :: Application
    serverApp = serve postsAPI $ server dir
