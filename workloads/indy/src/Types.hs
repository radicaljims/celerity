{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types (FSEvent, fsevents1) where

import Data.Aeson
import Data.List
import Data.Time.Calendar
import GHC.Generics
import Test.QuickCheck (Gen, choose, elements, listOf, listOf1, resize)
import Test.QuickCheck.Arbitrary

data FSEvent =
  FSEvent { eventType :: String
          , filePath :: String
          }
  deriving (Eq,Show,Generic)

-- https://gist.github.com/roman/1252086/432097a8a2f519bcf861578818b8096f60d22626
genWord :: Gen String
genWord = listOf1 (choose ('a', 'z'))

genPath :: Gen String
genPath = resize 10 (intercalate "/" <$> listOf genWord)

instance Arbitrary FSEvent where
  arbitrary = FSEvent <$> elements ["addormod", "delete"] <*> genPath

instance ToJSON FSEvent

fsevents1 :: [FSEvent]
fsevents1 =
  [FSEvent "addormod" "/tmp/hi.there"
  ,FSEvent "delete" "/tmp/good.bye"]
