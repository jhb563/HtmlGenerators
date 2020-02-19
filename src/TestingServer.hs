{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TestingServer where

import Data.Aeson
import qualified Data.Map as M
import Data.Proxy
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server

data User = User
  { userId :: Int
  , userName :: String
  , userEmail :: String
  , userAge :: Int
  } deriving (Show, Eq)

instance ToJSON User where
  toJSON u = object
    [ "id" .= userId u
    , "name" .= userName u
    , "email" .= userEmail u
    , "age" .= userAge u
    ]

type MyAPI = "users"  :> Capture "uid" Int :> Get '[JSON] (Maybe User)

userHandler :: Int -> Handler (Maybe User)
userHandler uid = return $ M.lookup uid userDB

myServer :: Server MyAPI
myServer = userHandler

myApi :: Proxy MyAPI
myApi = Proxy

myApp :: Application
myApp = serve myApi myServer

runServer :: IO ()
runServer = run 8080 myApp

userDB :: M.Map Int User
userDB = M.fromList
  [ (1, User 1 "James" "james@test.com" 25)
  , (2, User 2 "Adrian" "adrian@test.com" 30)
  , (3, User 3 "Valerie" "val@test.com" 28)
  , (4, User 4 "Katie" "kt@test.com" 21)
  ]
