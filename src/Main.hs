{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App.Html.Mustache
import App.Markdown
import App.Post
import Data.Maybe
import Data.Text hiding (filter, map)
import System.Directory
import System.Environment
import System.FilePath

main :: IO ()
main = do
  [srcDir] <- getArgs

  prepareDirs srcDir

  posts <- mdsToPosts srcDir
  generateIndexPage srcDir posts
  generatePostPages srcDir posts
  generateTagPages srcDir posts

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

mdsToPosts :: FilePath -> IO [Post]
mdsToPosts srcDir = do
  mds <- mdFiles (joinPath [srcDir, "posts"]) >>= mapM readFile
  return $
    fromMaybe
      (Post "" Nothing Nothing (Just []) (Markdown ""))
      . mdToPost
      . pack
      <$> mds

mdFiles :: FilePath -> IO [FilePath]
mdFiles dir = do
  files <- listDirectory dir
  let filesWithDir = map (dir </>) files
  return $ filter (isExtensionOf ".md") filesWithDir
