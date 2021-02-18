{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Post where

import Data.Aeson
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Time

data Post = Post
  { title :: T.Text,
    description :: Maybe T.Text,
    created :: Maybe UTCTime,
    tags :: Maybe [T.Text],
    draft :: Maybe Bool,
    key :: T.Text,
    standalone :: Maybe Bool,
    content :: Content
  }
  deriving (Show)

newtype Content = Markdown T.Text deriving (Show)

instance Eq Post where
  p == p' = title p == title p'

instance Ord Post where
  compare p p' = compare (created p) (created p')

instance ToJSON Post where
  toJSON Post {..} =
    object
      [ "title" .= title,
        "description" .= fromMaybe "" description,
        "created" .= created,
        "tags" .= fromMaybe [] tags,
        "draft" .= (Just True == draft),
        "key" .= key,
        "standalone" .= (Just True == standalone),
        "content" .= content
      ]

instance ToJSON Content where
  toJSON (Markdown md) = toJSON md

slug :: T.Text -> T.Text
slug = T.toLower . T.map dashIfNotAlphaNum
  where
    dashIfNotAlphaNum c = if isAlphaNum c then c else '-'

withSlug :: T.Text -> [Post] -> Maybe Post
withSlug s ps = case filter (\p -> s == key p) ps of
  [] -> Nothing
  (p' : _) -> Just p'

newestFirst :: [Post] -> [Post]
newestFirst = reverse . oldestFirst

oldestFirst :: [Post] -> [Post]
oldestFirst = sort

allTags :: [Post] -> [T.Text]
allTags [] = []
allTags [p] = tags' p
allTags (p : ps) = nub $ tags' p ++ allTags ps

withTag :: T.Text -> [Post] -> [Post]
withTag t = filter (\p -> t `elem` tags' p)

tags' :: Post -> [T.Text]
tags' = fromMaybe [] . tags

standalones :: [Post] -> [Post]
standalones = filter standalone'

notStandalones :: [Post] -> [Post]
notStandalones = filter (not . standalone')

standalone' :: Post -> Bool
standalone' = fromMaybe False . standalone

drafts :: [Post] -> [Post]
drafts = filter draft'

notDrafts :: [Post] -> [Post]
notDrafts = filter (not . draft')

draft' :: Post -> Bool
draft' = fromMaybe False . draft
