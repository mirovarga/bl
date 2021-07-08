{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Options.Generic
import SSG
import Static

main :: IO ()
main = runCommand =<< unwrapRecord "bl 0.6.1 [github.com/mirovarga/bl]"

runCommand :: Command Unwrapped -> IO ()
runCommand (Build dir) = generateHtml dir
runCommand (Serve dir port False) = serveStatic dir port
runCommand (Serve dir port True) = generateHtml dir >> serveStatic dir port

data Command w
  = Build
      { dir :: w ::: String <?> "Path to the directory with posts and templates (default: .)" <!> "."
      }
  | Serve
      { dir :: w ::: String <?> "Path to the directory with posts and templates (default: .)" <!> ".",
        port :: w ::: Int <?> "Port to listen on (default: 2703)" <!> "2703",
        rebuild :: w ::: Bool <?> "Rebuild before serving (default: False)" <!> "False"
      }
  deriving (Generic)

deriving instance Show (Command Unwrapped)

instance ParseRecord (Command Wrapped) where
  parseRecord =
    parseRecordWithModifiers
      defaultModifiers
        { shortNameModifier = firstLetter
        }
