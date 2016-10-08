{-# LANGUAGE OverloadedStrings #-}
import Api (indyAPI, fsEventsAPI)
import Server (indyServer, server1)
import Test.Hspec
import Test.QuickCheck.Instances
import Servant.QuickCheck
import Test.QuickCheck (Arbitrary(..))

spec :: Spec
spec = describe "check indy api" $ do
  let ioServer = do
        return $ indyServer

  it "should not return 500s" $ do
    withServantServer indyAPI ioServer $ \url ->
      serverSatisfies indyAPI url defaultArgs (not500 <%> mempty)

  it "should not return top-level json" $ do
    withServantServer indyAPI ioServer $ \url ->
      serverSatisfies indyAPI url defaultArgs (onlyJsonObjects <%> mempty)

  it "should return valid locations for 201" $ do
    withServantServer indyAPI ioServer $ \url ->
      serverSatisfies indyAPI url defaultArgs (createContainsValidLocation <%> mempty)

main :: IO ()
main = do
  hspec spec
