#! /usr/bin/env runhaskell
module Main where

import Options.Applicative
import qualified Types (Options(..))

import Filesystem

options :: Parser Types.Options
options = Types.Options
  <$> strOption
      (  long "directory"
      <> metavar "DIRECTORY"
      <> help "Directory to watch for changes" )
  <*> strOption
      (  long "share"
      <> metavar "SHARE"
      <> help "Location to copy modified files to")

main :: IO ()
main = execParser opts >>= watchForEvents
  where
    opts = info (helper <*> options)
      (  fullDesc
      <> progDesc "uatu sees all, copies all"
      <> header "uatu")
