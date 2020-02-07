module Main where

import System.IO

import BlazeLib

main :: IO ()
main = writeFile "hello.html" producePage
