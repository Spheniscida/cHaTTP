module Main where

import Chattp.PersistencePg.Config
import Chattp.PersistencePg.Talk

import Control.Monad (replicateM_)

import Control.Concurrent (forkIO)

import Network (Socket)
import Network.Socket.ByteString

main :: IO ()
main = do
    sock <- makePersistenceSocket
    brokeraddr <- makeBrokerSockAddr
    replicateM_ 2 (forkIO $ workerThread sock) -- 2 + 1 = 3 threads
    workerThread sock

workerThread :: Socket -> IO ()
workerThread mysock = do
    pgconn <- pgConnection
    (msg,sender) <- recvFrom mysock 16384
    resp <- handleMessage pgconn msg
    case resp of
        Nothing -> workerThread mysock
        Just r -> sendTo mysock r sender >> workerThread mysock

