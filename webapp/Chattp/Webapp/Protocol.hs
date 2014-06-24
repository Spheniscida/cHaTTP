{-# LANGUAGE OverloadedStrings #-}

module Chattp.Webapp.Protocol where

import Data.Maybe
import Data.Foldable
import qualified Data.Vector as V

import Data.Sequence()

import Data.Aeson
import Data.Aeson.Types(Pair)

import Data.Text.Lazy.Encoding

import qualified Data.ByteString.Lazy.Char8 as BS

import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.WireMessage

import qualified Chattp.WebappResponseMessage as Rp
import Chattp.WebappResponseMessage(WebappResponseMessage)
import Chattp.WebappResponseMessage.WebappResponseType

import qualified Chattp.ChattpMessage as Msg
import Chattp.ChattpMessage(ChattpMessage)

type SequenceNumber = Int
type UserName = BS.ByteString

{-
data WebappRequestMessage = WebappRequestMessage{sequence_number :: !P'.Word64,
                                                 type' :: !Chattp.WebappRequestMessage.WebappRequestType,
                                                 user_name :: !(P'.Maybe P'.Utf8), password :: !(P'.Maybe P'.Utf8),
                                                 channel_id :: !(P'.Maybe P'.Utf8), mesg :: !(P'.Maybe Chattp.ChattpMessage)}
                          deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

data WebappResponseMessage = WebappResponseMessage{sequence_number :: !P'.Word64,
                                                   type' :: !Chattp.WebappResponseMessage.WebappResponseType,
                                                   status :: !(P'.Maybe P'.Bool), online :: !(P'.Maybe P'.Bool),
                                                   authorized :: !(P'.Maybe P'.Bool), channel_id :: !(P'.Maybe P'.Utf8),
                                                   error_message :: !(P'.Maybe P'.Utf8), error_code :: !(P'.Maybe P'.Word32),
                                                   mesgs :: !(P'.Seq Chattp.ChattpMessage)}
                           deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

data ChattpMessage = ChattpMessage{sender :: !P'.Utf8, receiver :: !P'.Utf8, timestamp :: !P'.Utf8, body :: !(P'.Maybe P'.Utf8),
                                   group_message :: !(P'.Maybe P'.Bool), is_typing :: !(P'.Maybe P'.Bool),
                                   has_seen :: !(P'.Maybe P'.Bool)}
                   deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
-}


------------- Parse answers ---------------

parseAnswer :: BS.ByteString -> Either String WebappResponseMessage
parseAnswer input = case messageGet input of
                        Left e -> Left e
                        Right (m,rest) | rest == BS.empty -> Right m
                                       | otherwise -> Left "Couldn't parse completely"

-- JSON responses to web clients

responseToJSON :: WebappResponseMessage -> BS.ByteString
responseToJSON rp | Rp.type' rp == REGISTERED = encode . object $ makeJSON ("registered",rp)
                  | Rp.type' rp == LOGGEDIN   = encode . object $ ("channel_id" .= maybe "" uToString (Rp.channel_id rp)) : makeJSON ("logged-in",rp)
                  | Rp.type' rp == LOGGEDOUT  = encode . object $ makeJSON ("logged-out",rp)
                  | Rp.type' rp == SENTMESSAGE= encode . object $ makeJSON ("message-accepted",rp)
                  | Rp.type' rp == USERSTATUS = encode . object $ ("online" .= fromMaybe False (Rp.online rp)) : makeJSON ("isonline",rp)
                  | Rp.type' rp == GOTMESSAGES= encode . object $ ("messages" .= messagesToJSON (Rp.mesgs rp)) : makeJSON ("saved-messages",rp)
                  | Rp.type' rp == SAVEDSETTINGS= encode . object $ makeJSON ("saved-settings",rp)
                  | Rp.type' rp == GOTSETTINGS= encode . object $ ("settings" .= maybe "" uToString (Rp.settings rp)) : makeJSON ("settings",rp)
                  | otherwise = encode . object $ makeJSON ("unknown",rp)

makeJSON :: (String,WebappResponseMessage) -> [Pair]
makeJSON (type_,rp) = ["status" .= fromMaybe True (Rp.status rp),
                       "type"   .= type_,
                       "error"  .= maybe "" uToString (Rp.error_message rp),
                       "error-code".= fromMaybe 0 (Rp.error_code rp)
                      ]

-- Convert a Seq from message broker to a JSON value.
messagesToJSON :: Seq ChattpMessage -> Value
messagesToJSON msgs = Array . V.fromList . map messageToJSON . toList $ msgs

-- Schema/fields: from::String, to::String, timestamp::String, message::String, is_typing::Bool, has_seen::Bool, group_message::Bool
messageToJSON :: ChattpMessage -> Value
messageToJSON m = object [ "from"   .= uToString (Msg.sender m),
                           "to"     .= uToString (Msg.receiver m),
                           "timestamp".= uToString (Msg.timestamp m),
                           "group_message".= fromMaybe False (Msg.group_message m),
                           "body"   .= maybe "" (decodeUtf8 . utf8) (Msg.body m)
                           ]

----------- Helpers ------------------

unsafeToUtf8 :: BS.ByteString -> Utf8
unsafeToUtf8 b = case toUtf8 b of
                    Right u -> u
                    Left _i -> uFromString ""

