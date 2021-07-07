{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Options.Generic
import SSG

main :: IO ()
main = runCommand =<< unwrapRecord "bl 0.5.0 [github.com/mirovarga/bl]"

runCommand :: Command Unwrapped -> IO ()
runCommand (Build dir) = SSG.generateHtml dir

newtype Command w = Build
  { dir :: w ::: String <?> "Path to the directory with posts and templates (default: .)" <!> "."
  }
  deriving (Generic)

deriving instance Show (Command Unwrapped)

instance ParseRecord (Command Wrapped) where
  parseRecord =
    parseRecordWithModifiers
      defaultModifiers
        { shortNameModifier = firstLetter
        }
