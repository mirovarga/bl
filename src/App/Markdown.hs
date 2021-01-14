{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Markdown (mdToPost) where

import App.Post
import Data.Aeson
import Data.Text hiding (last)
import Data.Time
import GHC.Generics
import Text.MMark

mdToPost :: Text -> Maybe Post
mdToPost = mdToPost' . parseYamlMd

data FrontMatter = FrontMatter
  { title :: Text,
    created :: Maybe UTCTime,
    tags :: Maybe [Text]
  }
  deriving (Show, FromJSON, Generic)

parseYamlMd :: Text -> (Maybe FrontMatter, Maybe Content)
parseYamlMd yamlMd = case parse "yamlMd" yamlMd of
  Left e -> error $ show e
  Right yaml -> case fromJSON <$> projectYaml yaml of
    Nothing -> (Nothing, Nothing)
    Just yamlMd' -> case yamlMd' of
      Error e -> error $ show e
      Success yaml' -> (Just yaml', (Just . Markdown . extractMd) yamlMd)
  where
    extractMd = strip . last . splitOn "---"

mdToPost' :: (Maybe FrontMatter, Maybe Content) -> Maybe Post
mdToPost' (Just (FrontMatter t c Nothing), Just co) = Just $ Post t c [] co
mdToPost' (Just (FrontMatter t c (Just ts)), Just co) = Just $ Post t c ts co
mdToPost' (Nothing, _) = Nothing
mdToPost' (_, Nothing) = Nothing
