module Chattp.Webapp.Protocol where

import Text.Parsec
import Text.Parsec.ByteString.Lazy

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Builder
import Data.Monoid

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
bldNewline = char8 '\n'

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

parseAnswer :: BS.ByteString -> Either ParseError BrokerAnswerMessage
parseAnswer raw = parse protocolParser "proto-msg" raw

protocolParser :: Parser BrokerAnswerMessage
protocolParser = do
    seqn_str <- many1 (digit)
    char '\n'
    response_type <- choice [try (string "UONL") >> return UONL,
                            string "ACCMSG" >> return ACCMSG,
                            try (string "LGDIN") >> return LGDIN,
                            string "LGDOUT" >> return LGDOUT,
                            string "UREGD" >> return UREGD]
    char '\n'
    parseRest (read seqn_str) response_type

parseRest :: Int -> BrokerAnswerType -> Parser BrokerAnswerMessage
parseRest seqn UONL = do
    status <- choice [string (show ONLINE) >> return ONLINE,
                      string (show OFFLINE) >> return OFFLINE]
    return $ BrokerAnswerMessage seqn (UserStatus status)
parseRest seqn ACCMSG = do
    status <- parseStatus
    return $ BrokerAnswerMessage seqn (MessageAccepted status)
parseRest seqn LGDIN = do
    status <- parseStatus
    if status /= FAIL
     then do
        char '\n'
        chanid <- many1 lower
        return $ BrokerAnswerMessage seqn (UserLoggedIn status (Just (BS.pack chanid)))
     else return $ BrokerAnswerMessage seqn (UserLoggedIn status Nothing)
parseRest seqn LGDOUT = do
    status <- parseStatus
    return $ BrokerAnswerMessage seqn (UserLoggedOut status)
parseRest seqn UREGD = do
    status <- parseStatus
    return $ BrokerAnswerMessage seqn (UserRegistered status)

parseStatus :: Parser AnswerStatus
parseStatus = choice [string (show OK) >> return OK, string (show FAIL) >> return FAIL]


