{-# LANGUAGE Rank2Types #-}

module Reservoir where

-- import System.Environment
import Data.ByteString
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Network.UDP
import Control.Monad.Trans.Resource

-- reservoir :: IO ()
-- reservoir = getArgs >>= writeLogs

writeLogs :: Producer (ResourceT IO) Message -> String -> IO ()
writeLogs s o = runResourceT $ (mapOutput transformPkts s) $$ sinkFile o

transformPkts :: Message -> ByteString
transformPkts (Message b _) = b
