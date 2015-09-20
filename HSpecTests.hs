{-# LANGUAGE Rank2Types #-}

module HSpecTests where

import Main
import Test.Hspec
import Data.Conduit
import Data.Conduit.Network.UDP
import Data.Conduit.List
import Data.ByteString
import Data.Word
import Control.Monad.Trans.Resource

-- For removing the test file
import System.Directory
import Control.Exception
import System.IO.Error

import Network.Socket

testReservoir :: IO ()
testReservoir
  = writeLogs (source :: Producer (ResourceT IO) Message) "test.out"
  where source = sourceList [Message bytes addr]
        bytes = (singleton (51 :: Word8))
        addr = SockAddrUnix "a unix addr"

main :: IO ()
main = hspec $ do
  describe "writeLogs" $ after_ (removeIfExists "test.out") $ do
    it "writes UDP messages to a log file" $ do
      testReservoir `shouldReturn` ()
  describe "openUDPSocket" $ do
    it "opens a UDP socket" $ do
      checkSocket (openUDPSocket 60000) `shouldReturn` 60000

checkSocket :: IO Socket -> IO PortNumber
checkSocket skt = do
  s <- skt
  b <- socketPort s
  return b

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
