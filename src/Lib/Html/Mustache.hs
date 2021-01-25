{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Html.Mustache
  ( IndexPage (IndexPage),
    PostPage (PostPage),
    TagPage (TagPage),
    prepareDirs,
    indexToFile,
    postToFile,
    tagToFile,
    copyAssets,
  )
where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Lib.Post
import Lucid
import System.Directory
import System.FilePath
import Text.MMark
import Text.Mustache

newtype IndexPage = IndexPage [Post]

newtype PostPage = PostPage Post

data TagPage = TagPage T.Text [Post]

newtype HtmlPost = HtmlPost Post

instance ToJSON HtmlPost where
  toJSON (HtmlPost p@Post {..}) =
    object
      [ "title" .= title,
        "description" .= description,
        "created" .= created,
        "tags"
          .= zipWith
            (\t t' -> object ["camelCase" .= t, "lowerCase" .= t'])
            (tags' p)
            (map T.toLower (tags' p)),
        "draft" .= draft,
        "key" .= key,
        "content" .= HtmlContent content
      ]

instance {-# OVERLAPPING #-} ToJSON (Maybe UTCTime) where
  toJSON (Just d) = toJSON . T.pack $ formatTime defaultTimeLocale "%b %e, %Y" d
  toJSON Nothing = ""

newtype HtmlContent = HtmlContent Content

instance ToJSON HtmlContent where
  toJSON (HtmlContent (Markdown md)) = case parse "md" md of
    Left e -> error $ show e
    Right md' -> toJSON . renderText $ render md'

prepareDirs :: FilePath -> IO ()
prepareDirs dir = do
  let staticDir = joinPath [dir, "static"]
  removePathForcibly staticDir
  createDirectoryIfMissing True $ joinPath [staticDir, "tags"]

indexToFile :: FilePath -> IndexPage -> IO ()
indexToFile dir (IndexPage posts) = do
  template <- compileMustacheDir "index" (joinPath [dir, "templates"])
  TLIO.writeFile
    (joinPath [dir, "static", "index.html"])
    $ renderMustache template $
      object
        [ "title" .= ("Miro Varga" :: T.Text),
          "posts" .= map HtmlPost posts
        ]

postToFile :: FilePath -> PostPage -> IO ()
postToFile dir (PostPage p) = do
  template <- compileMustacheDir "post" $ joinPath [dir, "templates"]
  TLIO.writeFile
    (joinPath [dir, "static", T.unpack (slug $ title p) <> ".html"])
    $ renderMustache template $ toJSON $ HtmlPost p

tagToFile :: FilePath -> TagPage -> IO ()
tagToFile dir (TagPage t posts) = do
  template <- compileMustacheDir "tag" $ joinPath [dir, "templates"]
  TLIO.writeFile
    (joinPath [dir, "static/tags/" <> T.unpack (T.toLower t) <> ".html"])
    $ renderMustache template $
      object
        [ "title" .= ("Posts tagged '" <> t <> "'" :: T.Text),
          "posts" .= map HtmlPost posts
        ]

copyAssets :: FilePath -> IO ()
copyAssets dir = do
  let templatesDir = joinPath [dir, "templates"]
  files <- listDirectory templatesDir
  let filesWithDir = map (templatesDir </>) files
  let assets = filter (not . isExtensionOf ".mustache") filesWithDir
  mapM_ (\a -> copyFile a $ joinPath [dir, "static", takeFileName a]) assets
