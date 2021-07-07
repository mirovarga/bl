module SSG (generateHtml) where

import Data.Maybe
import Markdown
import Mustache
import Post
import System.FilePath

generateHtml :: FilePath -> IO ()
generateHtml dir = do
  prepareDirs dir

  posts <-
    filter (not . fromMaybe False . draft)
      . catMaybes
      <$> mdDirToPosts (joinPath [dir, "posts"])

  generateIndexPage dir posts
  generatePostPages dir posts
  generateTagPages dir posts

  copyAssets dir

generateIndexPage :: FilePath -> [Post] -> IO ()
generateIndexPage srcDir =
  indexToFile srcDir
    . IndexPage
    . newestFirst
    . notStandalones

generatePostPages :: FilePath -> [Post] -> IO ()
generatePostPages srcDir = mapM_ (postToFile srcDir . PostPage)

generateTagPages :: FilePath -> [Post] -> IO ()
generateTagPages srcDir posts =
  mapM_
    (tagToFile srcDir . tagPages)
    (allTags posts)
  where
    tagPages tag =
      TagPage
        tag
        $ newestFirst
          . notStandalones
          $ withTag tag posts
