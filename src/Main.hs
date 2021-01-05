{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App.Html.Mustache
import App.Markdown as M
import App.Post
import Data.Text hiding (filter, map)
import System.Directory
import System.Environment
import System.FilePath

main :: IO ()
main = do
  [srcDir] <- getArgs

  mds <- mdFiles (joinPath [srcDir, "posts"]) >>= mapM readFile
  let posts = mdToPost . M.Markdown . pack <$> mds

  prepareDirs srcDir

  indexToFile srcDir . IndexPage $ newestFirst posts
  mapM_ (postToFile srcDir . PostPage) posts
  mapM_
    (tagToFile srcDir . (\t -> TagPage t (newestFirst $ withTag t posts)))
    (allTags posts)

  copyAssets srcDir

mdFiles :: FilePath -> IO [FilePath]
mdFiles dir = do
  files <- listDirectory dir
  let filesWithDir = map (dir </>) files
  return $ filter (isExtensionOf ".md") filesWithDir
