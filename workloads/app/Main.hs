#! /usr/bin/env runhaskell
module Main where

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE QuasiQuotes      #-}

import Data.String
import Control.Applicative
import System.Process
import Control.Monad
import Filesystem.Path
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath)
import Twitch

import Lib

-- We have this combined 'upsert' since either twitch or the other underlying mechanisms
-- will sometimes only return a 'modify' event when creating a new file
handleAddOrMod :: String -> IO ()
handleAddOrMod n = do
  print ("The following file was added or modified: " ++ n)

handleDel :: String -> IO ()
handleDel d = do
  print ("The following file was deleted: " ++ d)

opts = Twitch.Options NoLogger Nothing Nothing True NoDebounce 0 (10^(6 :: Int)) False

main :: IO ()
main = defaultMainWithOptions opts $ do
  (fromString "*.*") |> handleAddOrMod |- handleDel
