{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module BasicServant where

import Data.ByteString.Lazy as Lazy
import qualified Data.Map as M
import Data.Proxy
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles (serveDirectoryWebApp)

data User = User
  { userId :: Int
  , userName :: String
  , userEmail :: String
  , userAge :: Int
  }

data HTML = HTML

newtype RawHtml = RawHtml { unRaw :: Lazy.ByteString }

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

type MyAPI =
  "users"  :> Capture "uid" Int :> Get '[HTML] RawHtml :<|>
  Raw

userHandler :: Int -> Handler RawHtml
userHandler uid = do
  let maybeUser = M.lookup uid userDB
  return $ RawHtml $ renderBS (renderUser maybeUser)

myServer :: Server MyAPI
myServer =
  userHandler :<|>
  serveDirectoryWebApp "static"

renderUser :: Maybe User -> Html ()
renderUser maybeUser = html_ $ do
  head_ $ do
    title_ "User Page"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/styles.css"]
  body_ $ userBody
  where
    userBody = case maybeUser of
      Nothing -> div_ [class_ "login-message"] $ do
        p_ "You aren't logged in!"
        br_ []
        a_ [href_ "/login"] "Please login"
      Just u -> div_ [class_ "user-message"] $ do
        p_ $ toHtml ("Name: " ++ userName u)
        p_ $ toHtml ("Email: " ++ userEmail u)
        p_ $ toHtml ("Age: " ++ show (userAge u))

userDB :: M.Map Int User
userDB = M.fromList
  [ (1, User 1 "James" "james@test.com" 25)
  , (2, User 2 "Adrian" "adrian@test.com" 30)
  , (3, User 3 "Valerie" "val@test.com" 28)
  , (4, User 4 "Katie" "kt@test.com" 21)
  ]

runServer :: IO ()
runServer = run 8080 (serve (Proxy :: Proxy MyAPI) myServer)
