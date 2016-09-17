{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Types (FSEvent, Directory, FileContent, fsevents1, directory1, fsevents2, content1) where

import           Data.Aeson
import           Data.List
import           Data.Time.Calendar
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Test.QuickCheck           (Gen, choose, elements, listOf,
                                            listOf1, resize)
import           Test.QuickCheck.Arbitrary

data FSEvent =
  FSEvent { eventType :: String
          , filePath  :: String
          , timeStamp :: String
          }
  deriving (Eq,Show,Generic)


data Directory =
  Directory {directoryPath :: String}
  deriving (Eq,Show,Generic)

data FileContent =
  FileContent {content :: String}
  deriving (Eq,Show,Generic)

-- https://gist.github.com/roman/1252086/432097a8a2f519bcf861578818b8096f60d22626
genWord :: Gen String
genWord = listOf1 (choose ('a', 'z'))

genPath :: Gen String
genPath = resize 10 (intercalate "/" <$> listOf genWord)

getOffsetTime offset = show $ posixSecondsToUTCTime $ fromIntegral offset

instance Arbitrary FSEvent where
  arbitrary = do
    secs <- choose (10 :: Integer, 1000000000)
    ty <- elements["addormod", "delete"]
    path <- genPath
    return (FSEvent ty path (getOffsetTime secs))

instance ToJSON FSEvent

instance ToJSON Directory

instance Arbitrary Directory where
  arbitrary = do
    path <- genPath
    return (Directory path)

instance ToJSON FileContent
instance Arbitrary FileContent where
  arbitrary = return $ FileContent "content"

fsevents1 :: [FSEvent]
fsevents1 =
  [FSEvent "addormod" "/tmp/hi.there" (getOffsetTime 50000)
  ,FSEvent "delete" "/tmp/good.bye" (getOffsetTime 10000)]

fsevents2 :: [FSEvent]
fsevents2 =
  [FSEvent "addormod" "/tmp/hi.there" (getOffsetTime 50000)
  ,FSEvent "delete" "/tmp/good.bye" (getOffsetTime 10000)]

directory1 :: [Directory]
directory1 = [Directory "/home/jims"]

content1 :: FileContent
content1 = FileContent "content"
