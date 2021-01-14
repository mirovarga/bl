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
import qualified Data.Text.Lazy as TL
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
  layout <- compileMustacheFile $ joinPath [dir, "templates/layout.mustache"]
  index <- compileMustacheFile $ joinPath [dir, "templates/index.mustache"]

  TLIO.writeFile (joinPath [dir, "static/index.html"]) $
    renderMustache layout $
      object
        [ "title" .= ("Miro Varga" :: T.Text),
          "content" .= (renderMustache index $ object ["posts" .= toJSON posts] :: TL.Text)
        ]

postToFile :: FilePath -> PostPage -> IO ()
postToFile dir (PostPage p@(Post title _ _ _)) = do
  layout <- compileMustacheFile $ joinPath [dir, "templates/layout.mustache"]
  post <- compileMustacheFile $ joinPath [dir, "templates/post.mustache"]

  TLIO.writeFile (joinPath [dir, "static", T.unpack (slug title) <> ".html"]) $
    renderMustache layout $
      object
        [ "title" .= title,
          "content" .= (renderMustache post $ toJSON p :: TL.Text)
        ]

tagToFile :: FilePath -> TagPage -> IO ()
tagToFile dir (TagPage t posts) = do
  layout <- compileMustacheFile $ joinPath [dir, "templates/layout.mustache"]
  tag <- compileMustacheFile $ joinPath [dir, "templates/tag.mustache"]

  TLIO.writeFile (joinPath [dir, "static/tags/" <> T.unpack (T.toLower t) <> ".html"]) $
    renderMustache layout $
      object
        [ "title" .= title',
          "content"
            .= ( renderMustache tag $
                   object
                     [ "title" .= title',
                       "posts" .= toJSON posts
                     ] ::
                   TL.Text
               )
        ]
  where
    title' = "Posts tagged '" <> t <> "'" :: T.Text

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
        "created" .= created p,
        "tags" .= (map (slug . T.toLower) $ tags p :: [T.Text]),
        "content" .= content p,
        "slug" .= (slug $ title p :: T.Text)
      ]

instance {-# OVERLAPPING #-} ToJSON (Maybe UTCTime) where
  toJSON d = case d of
    Just d' -> toJSON . T.pack $ formatTime defaultTimeLocale "%b %e, %Y" d'
    Nothing -> ""

instance ToJSON Content where
  toJSON (Markdown md) = case parse "md" md of
    Left err -> error $ show err
    Right md -> toJSON . renderText $ render md

slug :: T.Text -> T.Text
slug = T.toLower . T.map dashIfNotAlphaNum
  where
    dashIfNotAlphaNum c = if isAlphaNum c then c else '-'
