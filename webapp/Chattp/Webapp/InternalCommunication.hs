module Chattp.Webapp.InternalCommunication where

import Chattp.WebappResponseMessage as Rp
import Chattp.WebappRequestMessage as Rq

import Chattp.Webapp.Protocol

import Control.Concurrent

import qualified Data.HashTable.IO as HT

type DispatcherTable = HT.BasicHashTable SequenceNumber (Chan WebappResponseMessage)

data CenterRequestOrResponse = RegisterSequenceNumber (SequenceNumber,Chan WebappResponseMessage) | BrokerCenterResponse WebappResponseMessage

data ChanInfo = ChanInfo { requestsAndResponsesToCenterChan :: Chan CenterRequestOrResponse,
                           brokerRequestChan :: Chan WebappRequestMessage,
                           sequenceCounterChan :: Chan (Chan SequenceNumber) }

-- This thread ensures unique sequence numbers.
sequenceNumberManager :: Chan (Chan SequenceNumber) -> IO ()
sequenceNumberManager = manager 1
    where manager i chan = do
                        backchan <- readChan chan
                        writeChan backchan i
                        manager (i+1) chan

-- This thread dispatches incoming messages to FCGI threads.
centerThread :: Chan CenterRequestOrResponse -> IO ()
centerThread centerchan = do
    ht <- HT.new :: IO DispatcherTable
    manager centerchan ht

    where manager :: Chan CenterRequestOrResponse -> DispatcherTable -> IO ()
          manager chan tbl = do -- Loop
            msg <- readChan chan
            case msg of
                RegisterSequenceNumber (seqn,backchan) -> HT.insert tbl seqn backchan
                BrokerCenterResponse rp -> sendResponseToFCGI tbl rp
            manager chan tbl
          ----------------------------------------------------------------------------------
          sendResponseToFCGI :: DispatcherTable -> WebappResponseMessage -> IO () -- Look up the channel of that sequence number and reply with the BrokerAnswer
          sendResponseToFCGI tbl rp = do
                backchan <- HT.lookup tbl (fromIntegral $ Rp.sequence_number rp)
                HT.delete tbl (fromIntegral $ Rp.sequence_number rp)
                case backchan of
                    Nothing -> return () -- "Dangling transaction"
                    Just fcgichan -> writeChan fcgichan rp

