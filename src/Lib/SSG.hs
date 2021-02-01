module Lib.SSG (generateHtml) where

import Data.Maybe
import Lib.Html.Mustache
import Lib.Markdown
import Lib.Post
import System.FilePath

generateHtml :: FilePath -> IO ()
generateHtml dir = do
  prepareDirs dir

  posts <- mdDirToPosts $ joinPath [dir, "posts"]
  let nonDraftPosts = filter (not . fromMaybe False . draft) $ catMaybes posts
  generateIndexPage dir nonDraftPosts
  generatePostPages dir nonDraftPosts
  generateTagPages dir nonDraftPosts

  copyAssets dir

generateIndexPage :: FilePath -> [Post] -> IO ()
generateIndexPage srcDir posts = indexToFile srcDir . IndexPage $ newestFirst posts

generatePostPages :: FilePath -> [Post] -> IO ()
generatePostPages srcDir = mapM_ (postToFile srcDir . PostPage)

generateTagPages :: FilePath -> [Post] -> IO ()
generateTagPages srcDir posts =
  mapM_
    (tagToFile srcDir . (\t -> TagPage t (newestFirst $ withTag t posts)))
    (allTags posts)
