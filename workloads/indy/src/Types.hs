{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Types (FSEvent, Directory, FileContent, fsevents1, directory1, fsevents2, content1) where

import           Control.Lens
import           Data.Aeson
import           Data.List
import           Data.Swagger
import           Data.Time.Calendar
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Test.QuickCheck           (Gen, choose, elements, listOf,
                                            listOf1, resize)
import           Test.QuickCheck.Arbitrary

import           Servant.Elm               (ElmType)
import           Servant.Swagger

data FSEvent =
  FSEvent { eventType :: String
          , filePath  :: String
          , timeStamp :: String
          }
  deriving (Eq,Show,Generic)

data Directory =
  Directory {directoryPath :: String, shortName :: String, usedSpace :: Int}
  deriving (Eq,Show,Generic)

data FileContent =
  FileContent {content :: String}
  deriving (Eq,Show,Generic)

-- json and elm
instance ToJSON FSEvent
instance ToJSON Directory
instance ToJSON FileContent

instance ElmType FSEvent
instance ElmType Directory
instance ElmType FileContent

-- quickcheck
-- https://gist.github.com/roman/1252086/432097a8a2f519bcf861578818b8096f60d22626
genWord :: Gen String
genWord = listOf1 (choose ('a', 'z'))

genName :: Gen String
genName = resize 8 (listOf1 (choose ('a', 'z')))

genPath :: Gen String
genPath = resize 10 (intercalate "/" <$> listOf genWord)

getOffsetTime offset = show $ posixSecondsToUTCTime $ fromIntegral offset

instance Arbitrary FSEvent where
  arbitrary = do
    secs <- choose (10 :: Integer, 1000000000)
    ty <- Test.QuickCheck.elements["addormod", "delete"]
    path <- genPath
    return (FSEvent ty path (getOffsetTime secs))

instance Arbitrary Directory where
  arbitrary = do
    path <- genPath
    shortName <- genName
    usedSpace <- choose (10000 :: Int, 1000000)
    return (Directory path shortName usedSpace)

instance Arbitrary FileContent where
  arbitrary = return $ FileContent "content"

-- valhalla stubs
fsevents1 :: [FSEvent]
fsevents1 =
  [FSEvent "addormod" "/tmp/hi.there" (getOffsetTime 50000)
  ,FSEvent "delete" "/tmp/good.bye" (getOffsetTime 10000)]

fsevents2 :: [FSEvent]
fsevents2 =
  [FSEvent "addormod" "/tmp/hi.there" (getOffsetTime 50000)
  ,FSEvent "delete" "/tmp/good.bye" (getOffsetTime 10000)]

directory1 :: [Directory]
directory1 = [Directory "/home/jims" "jims" 10000]

content1 :: FileContent
content1 = FileContent "content"

-- swagger instances
-- TODO: can we pass quickcheck objects as examples?
instance ToSchema FSEvent where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "A file system event"
    & mapped.schema.example ?~ toJSON (FSEvent "addormod" "/tmp/hi.there" "111000034")

instance ToSchema Directory where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "A watched directory"
    & mapped.schema.example ?~ toJSON (Directory "/tmp/" "Temp!" 10000)

instance ToSchema FileContent where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "The content of a file backup"
    & mapped.schema.example ?~ toJSON (FileContent "content!")
