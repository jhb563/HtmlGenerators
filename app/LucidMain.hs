module Main where

import Lucid

import LucidLib

main :: IO ()
main = renderToFile "hello.html" mainHtml
