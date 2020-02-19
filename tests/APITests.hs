{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Servant.QuickCheck
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import TestingServer

main :: IO ()
main = hspec $ do
  apiSpec
  quickcheckSpec

apiSpec :: Spec
apiSpec = with (return myApp) $ do
  describe "GET /users/1" $ do
    it "responds with 200" $ do
      get "/users/1" `shouldRespondWith` 200
    it "responds with a user" $ do
      get "/users/1" `shouldRespondWith` "{\"email\":\"james@test.com\",\"age\":25,\"name\":\"James\",\"id\":1}"
    it "responds with a user" $ do
      get "/users/1" `shouldRespondWith` [json|{email: "james@test.com", age: 25, name: "James", id: 1}|]
  describe "GET /users/5" $ do
    it "responds with null" $ do
      get "/users/5" `shouldRespondWith` "null"

quickcheckSpec :: Spec
quickcheckSpec =
  it "API has good properties" $
    withServantServer myApi (return myServer) $ \burl ->
      serverSatisfies myApi burl defaultArgs
        (not500 <%> notLongerThan 1000000000 <%> onlyJsonObjects <%> mempty)
