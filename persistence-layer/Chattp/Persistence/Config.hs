module Chattp.Persistence.Config
( ConnectInfo
, SockAddr
, getInterfaceSockAddr
, getRedisConnInfo
) where

import Control.Applicative
import Network.Socket
import System.Environment

import Database.Redis


getInterfaceSockAddr :: IO SockAddr
getInterfaceSockAddr = SockAddrUnix <$> getEnv "CHATTP_PERSISTENCE_LAYER_ADDRESS"

getRedisConnInfo :: IO ConnectInfo
getRedisConnInfo = do
    sock <- getEnv "CHATTP_REDIS_SOCK"
    return $ defaultConnectInfo {connectPort = UnixSocket sock}
