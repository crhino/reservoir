{-# LANGUAGE Rank2Types #-}

module Main where

import Reservoir
import Test.Hspec
import Data.Conduit
import Data.Conduit.Network.UDP
import Data.Conduit.List
import Data.ByteString
import Data.Word
import Network.Socket.Internal
import Control.Monad.Trans.Resource

-- For removing the test file
import System.Directory
import Control.Exception
import System.IO.Error

testReservoir :: IO ()
testReservoir
  = writeLogs (source :: Producer (ResourceT IO) Message) "test.out"
  where source = sourceList [Message bytes addr]
        bytes = (singleton (51 :: Word8))
        addr = SockAddrUnix "a unix addr"

main :: IO ()
main = hspec $ after_ (removeIfExists "test.out") $ do
  describe "Reservoir" $ do
    it "listens on a UDP socket" $ do
      testReservoir `shouldReturn` ()

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
