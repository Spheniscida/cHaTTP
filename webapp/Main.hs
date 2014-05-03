module Main where

import Chattp.Webapp.Conf
import Chattp.Webapp.IPC
import Chattp.Webapp.Protocol
import Chattp.Webapp.InternalCommunication
import Chattp.Webapp.FastCGI

import Control.Concurrent

import Network.FastCGI

main = do
    config <- getConfig
    putStrLn "configured."
    sock <- createWebappSocket config
    centerchan <- newChan
    seqchan <- newChan
    outgoingchan <- newChan
    let chaninfo = ChanInfo { requestsAndResponsesToCenterChan = centerchan,
                              brokerRequestChan = outgoingchan,
                              sequenceCounterChan = seqchan }
    mapM_ forkOS (replicate 2 (socketIncoming sock centerchan))
    mapM_ forkOS (replicate 2 (socketOutgoing config sock outgoingchan))
    forkOS (sequenceNumberManager seqchan)
    forkOS (centerThread centerchan)
    -- run FCGI threads from here
    runFastCGIConcurrent 50 (fcgiMain (redisConnPool config) chaninfo)
    return ()
