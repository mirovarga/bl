{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Markdown (mdToPost) where

import App.Post
import Data.Aeson
import Data.Text hiding (last)
import Data.Time
import GHC.Generics
import Text.MMark

mdToPost :: Text -> Maybe Post
mdToPost = mdToPost' . parseYamlMd

data Metadata = Metadata
  { title :: Text,
    description :: Maybe Text,
    created :: Maybe UTCTime,
    tags :: Maybe [Text],
    draft :: Maybe Bool
  }
  deriving (Show, FromJSON, Generic)

parseYamlMd :: Text -> (Maybe Metadata, Maybe Content)
parseYamlMd yamlMd = case parse "yamlMd" yamlMd of
  Left e -> error $ show e
  Right yaml -> case fromJSON <$> projectYaml yaml of
    Nothing -> (Nothing, Nothing)
    Just yamlMd' -> case yamlMd' of
      Error e -> error $ show e
      Success yaml' -> (Just yaml', (Just . Markdown . extractMd) yamlMd)
  where
    extractMd = strip . last . splitOn "---"

mdToPost' :: (Maybe Metadata, Maybe Content) -> Maybe Post
mdToPost' (Just Metadata {..}, Just content) =
  Just $ Post title description created tags draft content
mdToPost' (Nothing, _) = Nothing
mdToPost' (_, Nothing) = Nothing
