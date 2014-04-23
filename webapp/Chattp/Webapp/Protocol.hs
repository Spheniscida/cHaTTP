{-# LANGUAGE OverloadedStrings #-}

module Chattp.Webapp.Protocol where

import Data.Attoparsec.ByteString.Lazy hiding (satisfy)
import Data.Attoparsec.ByteString.Char8 hiding (parse,Result, Done, Fail)

import qualified Data.Text.Lazy.Encoding as T
import Data.Aeson.Encode
import Data.Aeson.Types hiding (Parser, parse)

import qualified Data.Char
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as SBS -- Strict ByteStrings
import Data.ByteString.Builder
import Data.Monoid

type ProtoParser = Parser BrokerAnswerMessage

-- Protocol types
-- cf. /doc/protocols/webapp-message-broker.mkd

type SequenceNumber = Int
type ProtocolMessage = BS.ByteString

type UserName = BS.ByteString
type Password = BS.ByteString
type ChannelID = BS.ByteString
type MessageContent = BS.ByteString


data BrokerRequestType = UREG | LOGIN | LOGOUT | SNDMSG | UONLQ deriving Show

data AnswerStatus = OK | FAIL deriving (Show,Eq)
data UserStatus = ONLINE | OFFLINE deriving Eq
data BrokerAnswerType = UREGD | LGDIN | LGDOUT | ACCMSG | UONL deriving Show

data BrokerRequestMessage = BrokerRequestMessage SequenceNumber BrokerRequest deriving (Show,Eq)

data BrokerRequest = RegisterUser UserName Password -- UREG
                  | Login UserName Password -- LOGIN
                  | Logout UserName ChannelID -- LOGOUT
                  | SendMessage UserName ChannelID UserName MessageContent -- SNDMSG
                  | QueryStatus UserName -- UONLQ
                  deriving (Show,Eq)

data BrokerAnswerMessage = BrokerAnswerMessage SequenceNumber BrokerAnswer deriving (Show,Eq)

data BrokerAnswer = UserRegistered AnswerStatus
                         | UserLoggedIn AnswerStatus (Maybe ChannelID)
                         | UserLoggedOut AnswerStatus
                         | MessageAccepted AnswerStatus
                         | UserStatus UserStatus
                         deriving (Show,Eq)

instance Show UserStatus where
    show ONLINE = "Y"
    show OFFLINE = "N"

----------- Assemble requests ------------
bldNewline :: Builder
bldNewline = Data.ByteString.Builder.char8 '\n'

bldBS :: BS.ByteString -> Builder
bldBS = lazyByteString

bldShow :: Show s => s -> Builder
bldShow = string8 . show

requestToByteString :: BrokerRequestMessage -> BS.ByteString
requestToByteString (BrokerRequestMessage seqn (RegisterUser name pwd)) = toLazyByteString $ bldShow seqn
                                                                            <> bldNewline
                                                                            <> bldShow UREG
                                                                            <> bldBS name
                                                                            <> bldNewline
                                                                            <> bldBS pwd
requestToByteString (BrokerRequestMessage seqn (Login name pwd)) = toLazyByteString $ bldShow seqn
                                                                            <> bldNewline
                                                                            <> bldShow LOGIN
                                                                            <> bldNewline
                                                                            <> bldBS name
                                                                            <> bldNewline
                                                                            <> bldBS pwd
requestToByteString (BrokerRequestMessage seqn (Logout name chanid)) = toLazyByteString $ bldShow seqn
                                                                            <> bldNewline
                                                                            <> bldShow LOGOUT
                                                                            <> bldNewline
                                                                            <> bldBS name
                                                                            <> bldNewline
                                                                            <> bldBS chanid
requestToByteString (BrokerRequestMessage seqn (SendMessage name chanid dst msg)) = toLazyByteString $ bldShow seqn
                                                                            <> bldNewline
                                                                            <> bldShow SNDMSG
                                                                            <> bldNewline
                                                                            <> bldBS name
                                                                            <> bldNewline
                                                                            <> bldBS chanid
                                                                            <> bldNewline
                                                                            <> bldBS dst
                                                                            <> bldNewline
                                                                            <> bldBS msg
requestToByteString (BrokerRequestMessage seqn (QueryStatus name)) = toLazyByteString $ bldShow seqn
                                                                            <> bldNewline
                                                                            <> bldShow UONLQ
                                                                            <> bldNewline
                                                                            <> bldBS name


------------- Parse answers ---------------

parseAnswer :: BS.ByteString -> Either String BrokerAnswerMessage
parseAnswer input = case parse protocolParser input of
                        Done _ answ -> Right answ
                        Fail _ _ err -> Left err

--------------------------------------------------
protocolParser :: ProtoParser
protocolParser = do
    seqn_str <- many1 digit
    char '\n'
    response_type <- choice [try (string "UONL") >> return UONL,
                            string "ACCMSG" >> return ACCMSG,
                            try (string "LGDIN") >> return LGDIN,
                            string "LGDOUT" >> return LGDOUT,
                            string "UREGD" >> return UREGD]
    char '\n'
    parseRest (read seqn_str) response_type

--------------------------------------------------
parseRest :: Int -> BrokerAnswerType -> ProtoParser
parseRest seqn UONL = do
    status <- choice [string (SBS.pack . show $ ONLINE) >> return ONLINE,
                      string (SBS.pack . show $ OFFLINE) >> return OFFLINE]
    return $ BrokerAnswerMessage seqn (UserStatus status)
parseRest seqn ACCMSG = do
    status <- parseStatus
    return $ BrokerAnswerMessage seqn (MessageAccepted status)
parseRest seqn LGDIN = do
    status <- parseStatus
    if status /= FAIL
     then do
        char '\n'
        chanid <- many1 (satisfy Data.Char.isLower)
        return $ BrokerAnswerMessage seqn (UserLoggedIn status (Just (BS.pack chanid)))
     else return $ BrokerAnswerMessage seqn (UserLoggedIn status Nothing)
parseRest seqn LGDOUT = do
    status <- parseStatus
    return $ BrokerAnswerMessage seqn (UserLoggedOut status)
parseRest seqn UREGD = do
    status <- parseStatus
    return $ BrokerAnswerMessage seqn (UserRegistered status)

-------------------------------------------------
parseStatus :: Parser AnswerStatus
parseStatus = choice [string (SBS.pack . show $ OK) >> return OK,
                      string (SBS.pack . show $ FAIL) >> return FAIL]

-- JSON responses

responseToJSON :: BrokerAnswer -> BS.ByteString
responseToJSON (UserLoggedIn OK (Just chan_id)) = encode $ object ["status" .= True, "channel_id" .= T.decodeUtf8 chan_id]
responseToJSON (UserLoggedIn _ _) = encode $ object ["type" .= T.decodeUtf8 "logged-in",
                                                     "status" .= False,
                                                     "channel_id" .= T.decodeUtf8 ""]
responseToJSON (UserRegistered status) = encode $ object ["type" .= T.decodeUtf8 "registered",
                                                          "status" .= if status == OK then True else False]
responseToJSON (UserLoggedOut status) = encode $ object ["type" .= T.decodeUtf8 "logged-out",
                                                         "status" .= if status == OK then True else False]
responseToJSON (MessageAccepted status) = encode $ object ["type" .= T.decodeUtf8 "message-accepted",
                                                           "status" .= if status == OK then True else False]
responseToJSON (UserStatus status) = encode $ object ["type" .= T.decodeUtf8 "isonline",
                                                      "status" .= if status == ONLINE then True else False]


