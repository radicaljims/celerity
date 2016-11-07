module Filesystem(watchForEvents) where

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE QuasiQuotes      #-}

import Data.String
import Data.Time.Clock.POSIX
import Control.Applicative
import System.Process
import System.Directory
import Control.Monad
import Prelude hiding (FilePath)
import Twitch

import Turtle hiding (empty)
import Filesystem.Path.CurrentOS as OS

import Valhalla
import qualified Types (Options(..))

-- NOTE: we only support copying files in watched directories, not subdirectories!

getTime = do
  posix <- getPOSIXTime
  return (show $ (posixSecondsToUTCTime posix))

-- TODO: share and directory to watch in need to be command line arguments
copyToShare :: String -> String -> IO ()
copyToShare share path = do
  timestamp <- getTime
  let
    -- TODO: would be better to get the mtime of the path, probably...
    filepath = (fromString path)
    name = (filename filepath) -- WARNING: we aren't stripping directories out, they could blow up here!
    shareprefix = (fromString share)
    -- this lovely bit has the effect of stripping off the 'drive' and the file parts of filepath,
    -- relativizing the directory and letting us move it under our share
    directorypath = shareprefix </> (foldr (</>) OS.empty (tail $ init $ splitDirectories $ directory filepath)) </> (fromString timestamp)
    sharepath = (shareprefix </> directorypath </> name)
    in
      do
        mktree directorypath
        cp filepath sharepath

-- We have this combined 'upsert' since either twitch or the other underlying mechanisms
-- will sometimes only return a 'modify' event when creating a new file
handleAddOrMod :: String -> String -> IO ()
handleAddOrMod s n = do
  print ("The following file was added or modified: " ++ n)
  copyToShare s n
  postEvent "addormod" n

handleDel :: String -> String -> IO ()
handleDel s d = do
  print ("The following file was deleted: " ++ d)
  postEvent "delete" d

opts = Twitch.Options NoLogger Nothing Nothing True Debounce 0.5 (10^(6 :: Int)) False

watchForEvents :: Types.Options -> IO ()
watchForEvents (Types.Options directory share) =  do
  print ("Watching directory " ++ (directory ++ "/*.txt"))
  print ("Copying to share " ++ share)
  setCurrentDirectory (fromString directory)
  defaultMainWithOptions opts $ do
    (fromString ("*")) |> (handleAddOrMod share) |- (handleDel share)
