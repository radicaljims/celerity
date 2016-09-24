{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           GHC.Generics (Generic)
import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (Proxy (Proxy), Spec (Spec), defElmImports, generateElmForAPI, specsToDir, specsToDir)

import Api
import Types

spec :: Spec
spec = Spec ["elm", "IndyApi"]
            (defElmImports
             : generateElmForAPI (Proxy :: Proxy API))

main :: IO ()
main = specsToDir [spec] "generated"
