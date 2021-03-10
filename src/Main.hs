{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import qualified Lib.SSG as SSG
import qualified Lib.Server.API as API
import Options.Generic

main :: IO ()
main = runCommand =<< unwrapRecord "bl 0.4.0 [github.com/mirovarga/bl]"

runCommand :: Command Unwrapped -> IO ()
runCommand (Build dir) = SSG.generateHtml dir
runCommand (RESTAPI dir port) = API.run dir port

data Command w
  = Build
      { dir :: w ::: String <?> "Path to the directory with posts and templates (default: .)" <!> "."
      }
  | RESTAPI
      { dir :: w ::: String <?> "Path to the directory with posts and templates (default: .)" <!> ".",
        port :: w ::: Int <?> "Port to listen on (default: 2703)" <!> "2703"
      }
  deriving (Generic)

deriving instance Show (Command Unwrapped)

instance ParseRecord (Command Wrapped) where
  parseRecord =
    parseRecordWithModifiers
      defaultModifiers
        { shortNameModifier = firstLetter
        }
