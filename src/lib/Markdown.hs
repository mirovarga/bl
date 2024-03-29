{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Markdown (mdDirToPosts) where

import Data.Aeson
import Data.Maybe
import Data.Text hiding (filter, last, map)
import qualified Data.Text.IO as TIO
import Data.Time
import GHC.Generics
import Post
import System.Directory
import System.FilePath
import Text.MMark

mdDirToPosts :: FilePath -> IO [Maybe Post]
mdDirToPosts dir = mdFiles dir >>= mapM mdFileToPost

mdFileToPost :: FilePath -> IO (Maybe Post)
mdFileToPost path = mdToPost <$> TIO.readFile path

mdFiles :: FilePath -> IO [FilePath]
mdFiles dir = do
  files <- listDirectory dir
  let filesWithDir = map (dir </>) files
  return $ filter (isExtensionOf ".md") filesWithDir

mdToPost :: Text -> Maybe Post
mdToPost = mdToPost' . parseYamlMd

data Metadata = Metadata
  { title :: Text,
    description :: Maybe Text,
    created :: Maybe UTCTime,
    tags :: Maybe [Text],
    draft :: Maybe Bool,
    key :: Maybe Text,
    standalone :: Maybe Bool
  }
  deriving (Show, FromJSON, Generic)

parseYamlMd :: Text -> (Maybe Metadata, Maybe Content)
parseYamlMd yamlMd = case parse "yamlMd" yamlMd of
  Left _ -> (Nothing, Nothing)
  Right yaml -> case fromJSON <$> projectYaml yaml of
    Nothing -> (Nothing, Nothing)
    Just (Error _) -> (Nothing, Nothing)
    Just (Success yaml') -> (Just yaml', (Just . Markdown . extractMd) yamlMd)
  where
    extractMd = strip . last . splitOn "---"

mdToPost' :: (Maybe Metadata, Maybe Content) -> Maybe Post
mdToPost' (Just Metadata {..}, Just content) =
  Just $ Post title description created tags draft key' standalone content
  where
    key' = fromMaybe (slug title) key
mdToPost' (Nothing, _) = Nothing
mdToPost' (_, Nothing) = Nothing
