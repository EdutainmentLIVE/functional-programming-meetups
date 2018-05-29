{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

import Data.Monoid (mconcat)

main :: IO ()
main = scotty 8002 $
  get "/:term" $ do
    beam <- param "term"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
