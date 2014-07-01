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
    replicateM_ 2 (forkIO $ workerThread sock) -- 2 + 1 = 3 threads
    workerThread sock

workerThread :: Socket -> IO ()
workerThread mysock = do
    pgconn <- pgConnection
    worker pgconn

    where worker pgconn = do
            (msg,sender) <- recvFrom mysock 16384
            resp <- handleMessage pgconn msg
            case resp of
                Nothing -> worker pgconn
                Just r -> sendTo mysock r sender >> worker pgconn
