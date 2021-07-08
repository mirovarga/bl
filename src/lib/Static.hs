module Static (serveStatic) where

import Data.Proxy
import Network.Wai.Handler.Warp
import Servant
import System.FilePath

type Static = Raw

serveStatic :: FilePath -> Int -> IO ()
serveStatic dir port = run port app
  where
    static :: Proxy Static
    static = Proxy

    app :: Application
    app = serve static $ serveDirectoryFileServer $ dir </> "static"
