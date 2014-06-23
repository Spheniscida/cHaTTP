module Main where

import Chattp.PersistencePg.Config
import Chattp.PersistencePg.Database

import Control.Monad (replicateM_)

import Control.Concurrent (forkIO)

import Network.Socket

main :: IO ()
main = do
    sock <- makePersistenceSocket
    brokeraddr <- makeBrokerSockAddr
    replicateM_ 2 (forkIO $ workerThread sock brokeraddr) -- 2 + 1 = 3 threads
    workerThread sock brokeraddr

workerThread :: Socket -> SockAddr -> IO ()
workerThread mysock brokeraddr = do
    pgconn <- pgConnection
    undefined

