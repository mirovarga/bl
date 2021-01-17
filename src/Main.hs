module Main (main) where

import App.Html.Mustache
import App.Markdown
import App.Post
import Data.Maybe
import System.Environment
import System.FilePath

main :: IO ()
main = do
  [srcDir] <- getArgs

  prepareDirs srcDir

  posts <- mdDirToPosts $ joinPath [srcDir, "posts"]
  let nonDraftPosts = filter (not . fromMaybe False . draft) $ catMaybes posts
  generateIndexPage srcDir nonDraftPosts
  generatePostPages srcDir nonDraftPosts
  generateTagPages srcDir nonDraftPosts

  copyAssets srcDir

generateIndexPage :: FilePath -> [Post] -> IO ()
generateIndexPage srcDir posts = indexToFile srcDir . IndexPage $ newestFirst posts

generatePostPages :: FilePath -> [Post] -> IO ()
generatePostPages srcDir = mapM_ (postToFile srcDir . PostPage)

generateTagPages :: FilePath -> [Post] -> IO ()
generateTagPages srcDir posts =
  mapM_
    (tagToFile srcDir . (\t -> TagPage t (newestFirst $ withTag t posts)))
    (allTags posts)
