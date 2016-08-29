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

handleAddOrMod :: String -> IO ()
handleAddOrMod n = do
  print "Adding the following file:"
  print n

-- handleMod :: String -> IO ()
-- handleMod m = do
--   print "The following file was modified:"
--   print m

handleDel :: String -> IO ()
handleDel d = do
  print "The following file was deleted:"
  print d

main :: IO ()
main = defaultMain $ do
  (fromString "*.*") |> handleAddOrMod |- handleDel
