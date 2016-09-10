module Format (jsonForEvent) where

{-# LANGUAGE OverloadedStrings      #-}

import Data.Aeson (toJSON, object, (.=), Value)
import Data.Text (pack)

-- TODO: let's have a proper type of events so it's not an evil string!
jsonForEvent :: String -> String -> Value
jsonForEvent eventType fileName =
  object [ pack "data" .= object [pack "eventType" .= eventType, pack "fileName" .= fileName]]