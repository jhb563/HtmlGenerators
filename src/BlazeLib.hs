{-# LANGUAGE OverloadedStrings #-}

module BlazeLib where

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty

producePage = renderHtml $ docTypeHtml $ do
  H.head $ do
    H.title "Random Stuff"
    link ! rel "stylesheet" ! href "styles.css"
  body $ do
    h1 "Welcome to our site!"
    h2 $ H.span "New user?"
    H.div ! class_ "create-user-form" $ do
      H.form ! action "createUser" $ do
        input ! type_ "text" ! name "username"
        input ! type_ "email" ! name "email"
        input ! type_ "password" ! name "password"
        input ! type_ "submit" ! name "submit"
    br
    h2 $ H.span "Returning user?"
    H.div ! class_ "login-user-form" $ do
      H.form ! action "login" $ do
        input ! type_ "email" ! name "email"
        input ! type_ "password" ! name "password"
        input ! type_ "submit" ! name "submit"

numbers :: Int -> Html
numbers n = do
  p "A list of natural numbers:"
  ul $ forM_ [1..n] (li . toHtml)

simpleImage :: Html
simpleImage = img ! src "foo.png" ! alt "A foo image."

parentAttributes :: Html
parentAttributes = p ! class_ "styled" $ em "Context here."

altParentAttributes :: Html
altParentAttributes = (p $ em "Context here.") ! class_ "styled"

data User = User
  { getUserName :: String
  , getPoints :: Int
  }

userInfo :: Maybe User -> Html
userInfo u = H.div ! A.id "user-info" $ case u of
  Nothing ->
    a ! href "/login" $ "Please login."
  Just user -> do
    "Logged in as "
    toHtml $ getUserName user
    ". Your points: "
    toHtml $ getPoints user
