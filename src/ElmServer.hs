{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ElmServer where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy as Lazy
import Data.Proxy
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles (serveDirectoryWebApp)

data HTML = HTML

newtype RawHtml = RawHtml { unRaw :: Lazy.ByteString }

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

type MyAPI =
  "index" :> Get '[HTML] RawHtml :<|>
  "static" :> Raw

loadIndex :: Handler RawHtml
loadIndex = do
  indexFile <- liftIO (Lazy.readFile "static/index.html")
  return $ RawHtml indexFile

myServer :: Server MyAPI
myServer =
  loadIndex :<|>
  serveDirectoryWebApp "static"

runServer :: IO ()
runServer = run 8080 (serve (Proxy :: Proxy MyAPI) myServer)
