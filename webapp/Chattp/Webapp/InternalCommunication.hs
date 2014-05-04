module Chattp.Webapp.InternalCommunication where

import Chattp.Webapp.Protocol

import Control.Concurrent

import qualified Data.HashTable.IO as HT

type DispatcherTable = HT.BasicHashTable SequenceNumber (Chan BrokerAnswer)

data CenterRequestOrResponse = FCGICenterRequest (SequenceNumber,Chan BrokerAnswer) | BrokerCenterResponse BrokerAnswerMessage

data ChanInfo = ChanInfo { requestsAndResponsesToCenterChan :: Chan CenterRequestOrResponse,
                           brokerRequestChan :: Chan BrokerRequestMessage,
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
                FCGICenterRequest (seqn,backchan) -> HT.insert tbl seqn backchan
                BrokerCenterResponse (BrokerAnswerMessage seqn answer) -> sendResponseToFCGI tbl seqn answer
            manager chan tbl
          ----------------------------------------------------------------------------------
          sendResponseToFCGI :: DispatcherTable -> SequenceNumber -> BrokerAnswer -> IO () -- Look up the channel of that sequence number and reply with the BrokerAnswer
          sendResponseToFCGI tbl seqn answer = do
                backchan <- HT.lookup tbl seqn
                HT.delete tbl seqn
                case backchan of
                    Nothing -> return () -- "Dangling transaction"
                    Just fcgichan -> writeChan fcgichan answer



