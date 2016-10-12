{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Types (FSEvent, WatchedDirectory, FileContent, FileSystem, fsevents1, directory1, fsevents2, content1, status1) where

import           Control.Lens
import           Control.Monad
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

data WatchedDirectory =
  WatchedDirectory {directoryPath :: String, shortName :: String, usedSpace :: Int}
  deriving (Eq,Show,Generic)

data FileContent =
  FileContent {content :: String}
  deriving (Eq,Show,Generic)

-- Of course in real life a filesystem is a tree of directories and files...
data FileSystem =
  FileSystem { files :: [String] }
  deriving (Eq, Show, Generic)

-- json and elm
instance ToJSON FSEvent
instance ToJSON WatchedDirectory
instance ToJSON FileContent
instance ToJSON FileSystem

instance ElmType FSEvent
instance ElmType WatchedDirectory
instance ElmType FileContent
instance ElmType FileSystem

-- quickcheck
-- https://gist.github.com/roman/1252086/432097a8a2f519bcf861578818b8096f60d22626
genWord :: Gen String
genWord = listOf1 (choose ('a', 'z'))

genName :: Gen String
genName = resize 8 (listOf1 (choose ('a', 'z')))

genNameExt = do
  name <- genName
  ext <- genExt
  return (name ++ ext)

genExt :: Gen String
genExt = Test.QuickCheck.elements [".txt", ".elm", ".hs", ".exe", ".png", ""]

genPath :: Gen String
genPath = resize 10 (intercalate "/" <$> listOf genWord)

getOffsetTime offset = show $ posixSecondsToUTCTime $ fromIntegral offset

instance Arbitrary FSEvent where
  arbitrary = do
    secs <- choose (10 :: Integer, 1000000000)
    ty <- Test.QuickCheck.elements["addormod", "delete"]
    path <- genPath
    ext <- genExt
    return (FSEvent ty (path ++ ext) (getOffsetTime secs))

instance Arbitrary WatchedDirectory where
  arbitrary = do
    path <- genPath
    shortName <- genName
    usedSpace <- choose (10000 :: Int, 1000000)
    return (WatchedDirectory path shortName usedSpace)

instance Arbitrary FileContent where
  arbitrary = return $ FileContent "content"

instance Arbitrary FileSystem where
  arbitrary = do
    files <- replicateM 5 genNameExt

    return $ FileSystem files

-- swagger instances
-- TODO: can we pass quickcheck objects as examples?
instance ToSchema FSEvent where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "A file system event"
    & mapped.schema.example ?~ toJSON (FSEvent "addormod" "/tmp/hi.there" "111000034")

instance ToSchema WatchedDirectory where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "A watched directory"
    & mapped.schema.example ?~ toJSON (WatchedDirectory "/tmp/" "Temp!" 10000)

instance ToSchema FileContent where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "The content of a file backup"
    & mapped.schema.example ?~ toJSON (FileContent "content!")

instance ToSchema FileSystem where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "The files in a watched directory"
    & mapped.schema.example ?~ toJSON (FileSystem ["hi.png"])

-- valhalla stubs
fsevents1 :: [FSEvent]
fsevents1 =
  [FSEvent "addormod" "/tmp/hi.there" (getOffsetTime 50000)
  ,FSEvent "delete" "/tmp/good.bye" (getOffsetTime 10000)]

fsevents2 :: [FSEvent]
fsevents2 =
  [FSEvent "addormod" "/tmp/hi.there" (getOffsetTime 50000)
  ,FSEvent "delete" "/tmp/good.bye" (getOffsetTime 10000)]

directory1 :: [WatchedDirectory]
directory1 = [WatchedDirectory "/home/jims" "jims" 10000]

content1 :: FileContent
content1 = FileContent "content"

status1 :: FileSystem
status1 = FileSystem ["hi.png"]
