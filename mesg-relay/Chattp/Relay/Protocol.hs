{-# LANGUAGE OverloadedStrings #-}

{-
    - This file contains functions implementing the different protocols, e.g. creating HTTP messages,
    - handling broker communication and JSON messages.
-}

module Chattp.Relay.Protocol where

import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Aeson

import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.WireMessage

import Chattp.MessageRelayRequest as Rq

import Chattp.ChattpMessage as Msg

type ChannelID = BS.ByteString
type UserName = BS.ByteString
type Message = BS.ByteString

type SequenceNumber = Int

{-
data MessageRelayRequest = MessageRelayRequest{sequence_number :: !P'.Word64,
                                               type' :: !Chattp.MessageRelayRequest.MessageRelayRequestType,
                                               channel_id :: !(P'.Seq P'.Utf8), mesg :: !(P'.Maybe Chattp.ChattpMessage)}
                         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

data MessageRelayResponse = MessageRelayResponse{sequence_number :: !P'.Word64,
                                                 type' :: !Chattp.MessageRelayResponse.MessageRelayResponseType,
                                                 status :: !(P'.Maybe P'.Bool)}
                         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

data ChattpMessage = ChattpMessage{sender :: !P'.Utf8, receiver :: !P'.Utf8, timestamp :: !P'.Utf8, body :: !(P'.Maybe P'.Utf8),
                                   group_message :: !(P'.Maybe P'.Bool), is_typing :: !(P'.Maybe P'.Bool),
                                   has_seen :: !(P'.Maybe P'.Bool)}
                         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
-}

parseRequest :: BS.ByteString -> Either String MessageRelayRequest
parseRequest b = case messageGet b of
                    Left e -> Left ("Could not parse MessageRelayRequest: " ++ e)
                    Right (rq,rst) | rst == BS.empty -> Right rq
                                   | otherwise -> Left "Could not parse MessageRelayRequest"

makeJSONMessage :: Bool -> ChattpMessage -> Value
makeJSONMessage ign msg = object [ "ignore" .= ign,
                                   "message" .= maybe "" uToString (Msg.body msg),
                                   "from" .= uToString (Msg.sender msg),
                                   "to" .= uToString (Msg.receiver msg),
                                   "timestamp" .= uToString (Msg.timestamp msg),
                                   "is_typing" .= fromMaybe False (Msg.is_typing msg),
                                   "group_message" .= fromMaybe False (Msg.group_message msg),
                                   "has_seen" .= fromMaybe False (Msg.has_seen msg)]

