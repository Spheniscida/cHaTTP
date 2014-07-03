module Main where

import Chattp.Webapp.Conf
import Chattp.Webapp.IPC
import Chattp.Webapp.Protocol
import Chattp.Webapp.InternalCommunication
import Chattp.Webapp.FastCGI

import Control.Concurrent
import Control.Concurrent.MVar

import Network.FastCGI

main = do
    config <- getConfig
    putStrLn "configured."
    sock <- createWebappSocket config
    centerchan <- newChan
    outgoingchan <- newChan
    seqc <- newMVar 1
    let chaninfo = ChanInfo { requestsAndResponsesToCenterChan = centerchan,
                              brokerRequestChan = outgoingchan,
                              sequenceCounter = seqc }
    mapM_ forkIO (replicate 4 (socketIncoming sock centerchan))
    mapM_ forkIO (replicate 4 (socketOutgoing config sock chaninfo))
    forkOS (centerThread centerchan)
    -- run FCGI threads from here
    runFastCGIConcurrent' forkIO 256 (fcgiMain chaninfo)
    return ()

