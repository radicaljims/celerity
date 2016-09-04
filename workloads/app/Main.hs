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
-- import Filesystem.Path
-- import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath)
import Twitch
import Network.Wreq (post, responseStatus, statusCode)
import Data.Aeson (toJSON, object, (.=), Value)
import Control.Lens((^.))

import Turtle
import System.FilePath

import Lib

-- TODO: let's have a proper type of events so it's not an evil string!
create_json :: String -> String -> Value
create_json event_type file_name = object [(fromString "data") .= object [(fromString "eventType") .= event_type, (fromString "fileName") .= file_name]]

-- TODO: need to consider the possible error modes here
postEvent event_type file_name = do
  r <- post "http://localhost:3000/fsevents" (create_json event_type file_name)
  print (r ^. responseStatus . statusCode)

-- We have this combined 'upsert' since either twitch or the other underlying mechanisms
-- will sometimes only return a 'modify' event when creating a new file
handleAddOrMod :: String -> IO ()
handleAddOrMod n = do
  print ("The following file was added or modified: " ++ n)
  print (takeFileName (fromString n))
  cp (fromString n) (fromString "/Users/jims/" Turtle.</> (fromString (takeFileName n)))
  postEvent "addormod" n

handleDel :: String -> IO ()
handleDel d = do
  print ("The following file was deleted: " ++ d)
  postEvent "delete" d

opts = Twitch.Options NoLogger Nothing Nothing True NoDebounce 0 (10^(6 :: Int)) False

main :: IO ()
main = defaultMainWithOptions opts $ do
  (fromString "*") |> handleAddOrMod |- handleDel
