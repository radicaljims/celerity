{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           GHC.Generics (Generic)
import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (Proxy (Proxy), Spec (Spec), ToElmType, defElmImports, generateElmForAPI, specsToDir, specsToDir)

import Api
import Types

spec :: Spec
spec = Spec ["Generated", "MyApi"]
            (defElmImports
             : generateElmForAPI (Proxy :: Proxy API))

main :: IO ()
main = specsToDir [spec] "temp"
