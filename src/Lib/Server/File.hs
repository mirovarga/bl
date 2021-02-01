{-# LANGUAGE DataKinds #-}

module Lib.Server.File (run) where

import qualified Network.Wai.Handler.Warp as W
import Servant hiding (Post)
import System.FilePath

type StaticAPI = Raw

run :: FilePath -> Int -> IO ()
run dir port = W.run port serverApp
  where
    staticAPI :: Proxy StaticAPI
    staticAPI = Proxy

    serverApp :: Application
    serverApp = serve staticAPI $ serveDirectoryFileServer $ joinPath [dir, "static"]
