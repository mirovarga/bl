{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Html.Mustache
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

import App.Post
import Data.Aeson
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO
import Data.Time
import Lucid
import System.Directory
import System.FilePath
import Text.MMark
import Text.Mustache

newtype IndexPage = IndexPage [Post]

newtype PostPage = PostPage Post

data TagPage = TagPage T.Text [Post]

prepareDirs :: FilePath -> IO ()
prepareDirs dir = do
  let staticDir = joinPath [dir, "static"]
  removePathForcibly staticDir
  createDirectoryIfMissing True $ joinPath [staticDir, "tags"]

indexToFile :: FilePath -> IndexPage -> IO ()
indexToFile dir (IndexPage posts) = do
  template <- compileMustacheDir "index" $ joinPath [dir, "templates"]

  TLIO.writeFile (joinPath [dir, "static/index.html"]) $
    renderMustache template $
      object
        [ "title" .= ("Miro Varga" :: T.Text),
          "posts" .= toJSON posts
        ]

postToFile :: FilePath -> PostPage -> IO ()
postToFile dir (PostPage p@(Post t _ _ _ _)) = do
  template <- compileMustacheDir "post" $ joinPath [dir, "templates"]

  TLIO.writeFile (joinPath [dir, "static", T.unpack (slug t) <> ".html"]) $
    renderMustache template $
      toJSON p

tagToFile :: FilePath -> TagPage -> IO ()
tagToFile dir (TagPage t posts) = do
  template <- compileMustacheDir "tag" $ joinPath [dir, "templates"]

  TLIO.writeFile (joinPath [dir, "static/tags/" <> T.unpack (T.toLower t) <> ".html"]) $
    renderMustache template $
      object
        [ "title" .= ("Posts tagged '" <> t <> "'" :: T.Text),
          "posts" .= toJSON posts
        ]

copyAssets :: FilePath -> IO ()
copyAssets dir = do
  let templatesDir = joinPath [dir, "templates"]
  files <- listDirectory templatesDir
  let filesWithDir = map (templatesDir </>) files
  let assets = filter (not . isExtensionOf ".mustache") filesWithDir
  mapM_ (\a -> copyFile a $ joinPath [dir, "static", takeFileName a]) assets

instance ToJSON Post where
  toJSON p =
    object
      [ "title" .= title p,
        "description" .= description p,
        "created" .= created p,
        "tags" .= (map (slug . T.toLower) $ tags' p :: [T.Text]),
        "content" .= content p,
        "slug" .= (slug $ title p :: T.Text)
      ]

instance {-# OVERLAPPING #-} ToJSON (Maybe UTCTime) where
  toJSON d = case d of
    Just d' -> toJSON . T.pack $ formatTime defaultTimeLocale "%b %e, %Y" d'
    Nothing -> ""

instance ToJSON Content where
  toJSON (Markdown md) = case parse "md" md of
    Left e -> error $ show e
    Right md' -> toJSON . renderText $ render md'

slug :: T.Text -> T.Text
slug = T.toLower . T.map dashIfNotAlphaNum
  where
    dashIfNotAlphaNum c = if isAlphaNum c then c else '-'
