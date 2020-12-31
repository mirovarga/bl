{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Markdown
  ( Markdown (Markdown),
    mdToPost,
  )
where

import App.Post as P
import Data.Char hiding (toLower)
import Data.Text hiding (filter, head)
import Data.Time
import Text.ParserCombinators.ReadP

newtype Markdown = Markdown Text deriving (Show)

mdToPost :: App.Markdown.Markdown -> Post
mdToPost (App.Markdown.Markdown md) = fst . head . readP_to_S postP $ unpack md

postP :: ReadP Post
postP = do
  title <- titleP
  created <- createdP
  tags <- option [] tagsP
  (App.Markdown.Markdown md) <- contentP
  return $ Post {title, created, tags, content = P.Markdown md}

titleP :: ReadP Text
titleP = do
  skipSpaces
  char '#'
  skipSpaces
  pack <$> manyTill printableP newlineP
  where
    newlineP = satisfy (== '\n')

createdP :: ReadP (Maybe UTCTime)
createdP = do
  skipSpaces
  char '*'
  parseDate . pack <$> manyTill printableP (char '*')
  where
    parseDate = parseTimeM True defaultTimeLocale "%b %e, %Y" . unpack

tagsP :: ReadP [Text]
tagsP = do
  skipSpaces
  char '*'
  tags <- pack <$> manyTill printableP (char '*')
  return $ Prelude.map strip (splitOn "," tags)

contentP :: ReadP App.Markdown.Markdown
contentP = do
  skipSpaces
  App.Markdown.Markdown . pack <$> manyTill p eof
  where
    p = satisfy (\c -> isPrint c || c == '\n')

printableP :: ReadP Char
printableP = satisfy isPrint
