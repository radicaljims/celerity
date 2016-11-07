module Types(Options(..)) where

import Options.Applicative

data Options = Options
  { directory :: String, share :: String }
