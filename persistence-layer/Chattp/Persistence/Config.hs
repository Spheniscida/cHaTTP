{-# LANGUAGE TupleSections #-}

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


getInterfaceSockAddr :: IO (Family, SockAddr)
getInterfaceSockAddr = getInterfaceSockAddr' =<< getEnv "CHATTP_PERSISTENCE_LAYER_FAMILY"

getInterfaceSockAddr' :: String -> IO (Family, SockAddr)
getInterfaceSockAddr' "UNIX" = (AF_UNIX,) . SockAddrUnix <$> getEnv "CHATTP_PERSISTENCE_LAYER_ADDR"
getInterfaceSockAddr' "INET" = do
    host <- getEnv "CHATTP_PERSISTENCE_LAYER_ADDR"
    port <- getEnv "CHATTP_PERSISTENCE_LAYER_PORT"
    addrInfos <- getAddrInfo Nothing (Just host) (Just port)
    return . (AF_INET6,) . addrAddress $ head addrInfos


getRedisConnInfo :: IO ConnectInfo
getRedisConnInfo = getRedisConnInfo' =<< getEnv "CHATTP_REDIS_FAMILY"

getRedisConnInfo' :: String -> IO ConnectInfo
getRedisConnInfo' "UNIX" = do
    addr <- getEnv "CHATTP_REDIS_ADDR"
    return $ defaultConnectInfo {connectPort = UnixSocket addr}
getRedisConnInfo' "INET" = do
    addr <- getEnv "CHATTP_REDIS_ADDR"
    port <- getEnv "CHATTP_REDIS_PORT"
    return $ defaultConnectInfo {connectHost = addr, connectPort = Service port}
