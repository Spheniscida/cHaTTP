module Chattp.Webapp.InternalCommunication where

import Chattp.WebappResponseMessage as Rp
import Chattp.WebappRequestMessage as Rq

import Chattp.Webapp.Protocol

import Control.Concurrent

import qualified Data.HashTable.IO as HT

type DispatcherTable = HT.BasicHashTable SequenceNumber (MVar WebappResponseMessage)

data CenterRequestOrResponse = RegisterSequenceNumber (SequenceNumber,MVar WebappResponseMessage) | BrokerCenterResponse WebappResponseMessage

data ChanInfo = ChanInfo { requestsAndResponsesToCenterChan :: Chan CenterRequestOrResponse,
                           brokerRequestChan :: Chan WebappRequestMessage,
                           sequenceCounter :: MVar SequenceNumber }

-- This thread dispatches incoming messages to FCGI threads.
centerThread :: Chan CenterRequestOrResponse -> IO ()
centerThread centerchan = do
    ht <- HT.new :: IO DispatcherTable
    manager centerchan ht

    where manager :: Chan CenterRequestOrResponse -> DispatcherTable -> IO ()
          manager chan tbl = do -- Loop
            msg <- readChan chan
            case msg of
                RegisterSequenceNumber (seqn,backvar) -> HT.insert tbl seqn backvar
                BrokerCenterResponse rp -> sendResponseToFCGI tbl rp
            manager chan tbl
          ----------------------------------------------------------------------------------
          sendResponseToFCGI :: DispatcherTable -> WebappResponseMessage -> IO () -- Look up the channel of that sequence number and reply with the BrokerAnswer
          sendResponseToFCGI tbl rp = do
                backvar <- HT.lookup tbl (fromIntegral $ Rp.sequence_number rp)
                HT.delete tbl (fromIntegral $ Rp.sequence_number rp)
                case backvar of
                    Nothing -> return () -- "Dangling transaction"
                    Just v -> tryPutMVar v rp >> return () -- just fail silently if the MVar is full, for whatever reason

