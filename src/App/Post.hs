module App.Post where

import Data.List
import Data.Maybe
import Data.Text hiding (filter, reverse)
import Data.Time

data Post = Post
  { title :: Text,
    description :: Maybe Text,
    created :: Maybe UTCTime,
    tags :: Maybe [Text],
    draft :: Maybe Bool,
    content :: Content
  }
  deriving (Show)

newtype Content = Markdown Text deriving (Show)

instance Eq Post where
  p == p' = title p == title p'

instance Ord Post where
  compare p p' = compare (created p) (created p')

newestFirst :: [Post] -> [Post]
newestFirst = reverse . sort

allTags :: [Post] -> [Text]
allTags [] = []
allTags [p] = tags' p
allTags (p : ps) = nub $ tags' p ++ allTags ps

withTag :: Text -> [Post] -> [Post]
withTag t = filter (\p -> t `elem` tags' p)

tags' :: Post -> [Text]
tags' = fromMaybe [] . tags
