{-# LANGUAGE Rank2Types #-}

module Reservoir where

-- import System.Environment
import Data.ByteString
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Network.UDP
import Control.Monad.Trans.Resource
import Network.Socket

-- reservoir :: IO ()
-- reservoir = getArgs >>= writeLogs

writeLogs :: Producer (ResourceT IO) Message -> String -> IO ()
writeLogs s o = runResourceT $ (mapOutput transformPkts s) $$ sinkFile o

transformPkts :: Message -> ByteString
transformPkts (Message b _) = b

openUDPSocket :: PortNumber -> IO Socket
openUDPSocket port = do
  s <- socket AF_INET Datagram defaultProtocol
  bind s addr
  return s
  where addr = SockAddrInet port iNADDR_ANY
