{-# LANGUAGE Rank2Types #-}

module Main where

import System.Environment
import System.Exit
import Data.ByteString hiding (snoc)
import Data.ByteString.Char8 (snoc)
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Network.UDP
import Control.Monad.Trans.Resource
import Network.Socket

maxUDPMessage = 65535

main :: IO ()
main = do
  as <- getArgs
  case (Prelude.length as) of
    2 -> reservoir as
    _ -> do
          Prelude.putStrLn "Must provide port and file path"
          exitFailure

reservoir :: [String] -> IO ()
reservoir args = do
  s <- openUDPSocket port
  writeLogs (sourceSocket s maxUDPMessage) file
  close s
  where port = fromInteger (read $ Prelude.head args :: Integer)
        file = Prelude.last args

writeLogs :: Producer (ResourceT IO) Message -> String -> IO ()
writeLogs s o = runResourceT $ (mapOutput transformPkts s) $$ sinkFile o

transformPkts :: Message -> ByteString
transformPkts (Message b _) = snoc b '\n'

openUDPSocket :: PortNumber -> IO Socket
openUDPSocket port = do
  s <- socket AF_INET Datagram defaultProtocol
  bind s addr
  return s
  where addr = SockAddrInet port iNADDR_ANY
