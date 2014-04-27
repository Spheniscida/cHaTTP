{-# LANGUAGE OverloadedStrings #-}

module Chattp.Relay.Protocol where

import Data.Monoid ((<>))

import qualified Data.ByteString.Lazy.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Attoparsec.ByteString.Lazy as AP hiding (satisfy,takeWhile)
import Data.Attoparsec.ByteString.Char8 as AP hiding (parse,Result,Done,Fail)

type ChannelID = BS.ByteString
type UserName = BS.ByteString
type Message = BS.ByteString

type SequenceNumber = Int

data Status = OK | FAIL deriving Show

data BrokerRequestMessage = BrokerRequestMessage SequenceNumber BrokerRequest deriving Show

data BrokerRequest = SNDMSG UserName ChannelID Message
                   | NEWCHAN ChannelID
                   | DELCHAN ChannelID deriving Show

data BrokerResponseMessage = BrokerResponseMessage SequenceNumber BrokerResponse deriving Show

data BrokerResponse = MSGSNT Status
                    | CHANCREAT Status
                    | DELTDCHAN Status deriving Show


-- Serializing

bldShow :: Show a => a -> BSB.Builder
bldShow = BSB.string8 . show

bldNL :: BSB.Builder
bldNL = BSB.char8 '\n'

responseToRaw :: BrokerResponseMessage -> BS.ByteString
responseToRaw (BrokerResponseMessage seqn (MSGSNT stat)) = BSB.toLazyByteString $
                                                            bldShow seqn
                                                         <> bldNL
                                                         <> BSB.string8 "MSGSNT"
                                                         <> bldNL
                                                         <> bldShow stat
responseToRaw (BrokerResponseMessage seqn (CHANCREAT stat)) = BSB.toLazyByteString $
                                                            bldShow seqn
                                                         <> bldNL
                                                         <> BSB.string8 "CHANCREAT"
                                                         <> bldNL
                                                         <> bldShow stat
responseToRaw (BrokerResponseMessage seqn (DELTDCHAN stat)) = BSB.toLazyByteString $
                                                            bldShow seqn
                                                         <> bldNL
                                                         <> BSB.string8 "DELTDCHAN"
                                                         <> bldNL
                                                         <> bldShow stat

-- Parsing

type ProtoParser = Parser BrokerRequestMessage

parseRequest :: BS.ByteString -> Either String BrokerRequestMessage
parseRequest rq = case parse brokerRequestParser rq of
                    Fail _ _ e -> Left e
                    Done _ r -> Right r

brokerRequestParser :: ProtoParser
brokerRequestParser = do
    seqn <- decimal
    char '\n'
    request <- choice [string "SNDMSG\n" >> parseSNDMSG,
                       string "NEWCHAN\n" >> parseNEWCHAN,
                       string "DELCHAN\n" >> parseDELCHAN]
    return (BrokerRequestMessage seqn request)

parseSNDMSG, parseNEWCHAN, parseDELCHAN :: Parser BrokerRequest

parseSNDMSG = do
    usr <- fmap BS.fromStrict (AP.takeWhile (/= '\n'))
    char '\n'
    channel <- fmap BS.fromStrict (AP.takeWhile (/= '\n'))
    char '\n'
    message <- fmap BS.fromStrict takeByteString
    return (SNDMSG usr channel message)

parseNEWCHAN = do
    channel <- takeByteString
    return (NEWCHAN $ BS.fromStrict channel)

parseDELCHAN = do
    channel <- takeByteString
    return (DELCHAN $ BS.fromStrict channel)

