{-# LANGUAGE OverloadedStrings #-}

module LucidLib where

import Lucid

mainHtml :: Html ()
mainHtml = html_ $ do
  head_ $ do
    title_ "Random Stuff"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "screen.css"]
  body_ $ do
    h1_ "Welcome to our site!"
    h2_ $ span_ "New user?"
    div_ [class_ "create-user-form"] $ do
      form_ [action_ "createUser"] $ do
        input_ [type_ "text", name_ "username"]
        input_ [type_ "email", name_ "email"]
        input_ [type_ "password", name_ "password"]
        input_ [type_ "submit", name_ "submit"]
    br_ []
    h2_ $ span_ "Returning user?"
    div_ [class_ "login-user-form"] $ do
      form_ [action_ "login"] $ do
        input_ [type_ "email", name_ "email"]
        input_ [type_ "password", name_ "password"]
        input_ [type_ "submit", name_ "submit"]
    br_ []

stylesheet :: Html ()
stylesheet = link_ [rel_ "stylesheet", type_ "text/css", href_ "screen.css"]

aDiv :: Html ()
aDiv = div_ $ p_ "Hello"

aDiv2 :: Html ()
aDiv2 = div_ [class_ "hello-div"] $ p_ "Hello"
