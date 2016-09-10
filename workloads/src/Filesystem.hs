module Filesystem(watchForEvents) where

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
import Prelude hiding (FilePath)
import Twitch

import Turtle hiding (empty)
import Filesystem.Path.CurrentOS as OS

import Valhalla

-- TODO: share and directory to watch in need to be command line arguments
copyToShare :: String -> IO ()
copyToShare path = do
  let filepath = (fromString path)
      name = (filename filepath)
      shareprefix = (fromString "/Users/jims/share")
      -- this lovely bit has the effect of stripping off the 'drive' and the file parts of filepath,
      -- relativizing the directory and letting us move it under our share
      directorypath = shareprefix </> (foldr (</>) OS.empty (tail $ init $ splitDirectories $ directory filepath))
      sharepath = (shareprefix </> directorypath </> name) in
        do
          mktree directorypath
          cp filepath sharepath

-- We have this combined 'upsert' since either twitch or the other underlying mechanisms
-- will sometimes only return a 'modify' event when creating a new file
handleAddOrMod :: String -> IO ()
handleAddOrMod n = do
  print ("The following file was added or modified: " ++ n)
  copyToShare n
  postEvent "addormod" n

handleDel :: String -> IO ()
handleDel d = do
  print ("The following file was deleted: " ++ d)
  postEvent "delete" d

opts = Twitch.Options NoLogger Nothing Nothing True Debounce 0.5 (10^(6 :: Int)) False

watchForEvents :: IO ()
watchForEvents = defaultMainWithOptions opts $ do
  (fromString "*") |> handleAddOrMod |- handleDel
