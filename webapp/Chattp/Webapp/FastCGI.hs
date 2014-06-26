{-# LANGUAGE OverloadedStrings #-}

module Chattp.Webapp.FastCGI where

import Chattp.Webapp.InternalCommunication
import Chattp.Webapp.Protocol

import Text.ProtocolBuffers.Header -- defaultValue

import Chattp.WebappResponseMessage as Rp
import Chattp.WebappRequestMessage as Rq
import Chattp.ChattpMessage as Msg

import Chattp.WebappRequestMessage.WebappRequestType as Rq
import Chattp.WebappResponseMessage.WebappResponseType as Rp

import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Encoding as TS

import Data.Aeson as AE

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Attoparsec.ByteString.Lazy
import Data.Attoparsec.ByteString.Char8 as AP8 hiding (parse, Done, Fail)

import Data.UnixTime

import Network.FastCGI
import Control.Concurrent.Chan

-- This function will be run by runFastCGIConcurrent

fcgiMain :: ChanInfo -> CGI CGIResult
fcgiMain channels = do
    Just doc_uri <- getVar "DOCUMENT_URI" -- document uri is /always/ there. Kill me if not ;)
    let rq_type = getOpType . BS.pack $ doc_uri
    setHeader "Content-Type" "application/json"
    case rq_type of
        WebLogin -> handleLogin channels
        WebLogout -> handleLogout channels
        WebRegister -> handleRegister channels
        WebSendMessage -> handleSendMessage channels
        WebStatusRequest -> handleStatusRequest channels
        WebMessagesRequest -> handleMessagesRequest channels
        WebConfSaveRequest -> handleConfSaveRequest channels
        WebConfGetRequest -> handleConfGetRequest channels
        VoidRequest s -> outputError 404 ("Malformed request: Unknown request type or parse failure: " ++ s) []

-- Op handlers

handleLogin :: ChanInfo -> CGI CGIResult
handleLogin chans = do
    usr_raw <- getInputFPS "user_name"
    pwd_raw <- getInputFPS "password"
    case (usr_raw,pwd_raw) of
        (Just usr, Just pwd) -> do
                    seqchan <- liftIO newChan
                    liftIO $ writeChan (sequenceCounterChan chans) seqchan
                    seqn <- liftIO $ readChan seqchan

                    answerchan <- liftIO newChan
                    liftIO $ writeChan (requestsAndResponsesToCenterChan chans) (RegisterSequenceNumber (seqn,answerchan))

                    let request = defaultValue { Rq.sequence_number = fromIntegral seqn,
                                                 Rq.type' = LOGIN,
                                                 Rq.user_name = Just $ unsafeToUtf8 usr,
                                                 Rq.password = Just $ unsafeToUtf8 pwd }
                    liftIO $ writeChan (brokerRequestChan chans) request

                    brokeranswer <- liftIO $ readChan answerchan

                    case Rp.type' brokeranswer of
                        LOGGEDIN -> do
                            let jsonresponse = responseToJSON brokeranswer
                            setHeader "Content-length" (show . BS.length $ jsonresponse)
                            outputFPS jsonresponse
                        _ -> outputError 500 "Sorry, this is an implementation error. [handleLogin,wrongAnswerType]" []
        _ -> outputError 400 "Login request lacking request parameter(s)" []

handleLogout :: ChanInfo -> CGI CGIResult
handleLogout chans = do
    usr_raw <- getInputFPS "user_name"
    channel_raw <- getInputFPS "channel_id"
    case (usr_raw,channel_raw) of
        (Just usr, Just channel) -> do
                    seqchan <- liftIO newChan
                    liftIO $ writeChan (sequenceCounterChan chans) seqchan
                    seqn <- liftIO $ readChan seqchan

                    answerchan <- liftIO newChan
                    liftIO $ writeChan (requestsAndResponsesToCenterChan chans) (RegisterSequenceNumber (seqn,answerchan))

                    let request = defaultValue { Rq.sequence_number = fromIntegral seqn,
                                                 Rq.type' = LOGOUT,
                                                 Rq.user_name = Just $ unsafeToUtf8 usr,
                                                 Rq.channel_id = Just $ unsafeToUtf8 channel }
                    liftIO $ writeChan (brokerRequestChan chans) request

                    brokeranswer <- liftIO $ readChan answerchan

                    case Rp.type' brokeranswer of
                        LOGGEDOUT -> do
                            let jsonresponse = responseToJSON brokeranswer
                            setHeader "Content-length" (show . BS.length $ jsonresponse)
                            outputFPS jsonresponse
                        _ -> outputError 500 "Sorry, this is an implementation error. [handleLogout,wrongAnswerType]" []
        _ -> outputError 400 "Logout request lacking request parameter(s)" []

handleRegister :: ChanInfo -> CGI CGIResult
handleRegister chans = do
    usr_raw <- getInputFPS "user_name"
    pwd_raw <- getInputFPS "password"
    case (usr_raw,pwd_raw) of
        (Just usr, Just pwd) -> do
                    seqchan <- liftIO $ newChan
                    liftIO $ writeChan (sequenceCounterChan chans) seqchan
                    seqn <- liftIO $ readChan seqchan

                    answerchan <- liftIO newChan
                    liftIO $ writeChan (requestsAndResponsesToCenterChan chans) (RegisterSequenceNumber (seqn,answerchan))

                    let request = defaultValue { Rq.sequence_number = fromIntegral seqn,
                                                 Rq.type' = REGISTER,
                                                 Rq.user_name = Just $ unsafeToUtf8 usr,
                                                 Rq.password = Just $ unsafeToUtf8 pwd }
                    liftIO $ writeChan (brokerRequestChan chans) request

                    brokeranswer <- liftIO $ readChan answerchan

                    case Rp.type' brokeranswer of
                        REGISTERED -> do
                            let jsonresponse = responseToJSON brokeranswer
                            setHeader "Content-length" (show . BS.length $ jsonresponse)
                            outputFPS jsonresponse
                        _ -> outputError 500 "Sorry, this is an implementation error. [handleRegister,wrongAnswerType]" []
        _ -> outputError 400 "Register request lacking request parameter(s)" []

handleSendMessage :: ChanInfo -> CGI CGIResult
handleSendMessage chans = do
    usr_raw <- getInputFPS "user_name"
    channel_raw <- getInputFPS "channel_id"
    dest_raw <- getInputFPS "dest_user"
    mesg <- getBodyFPS
    if mesg == BS.empty
     then outputError 400 "Empty message; not sent." []
     else case (usr_raw,dest_raw,channel_raw) of
        (Just usr, Just dst, Just channel) -> do
                    seqchan <- liftIO $ newChan
                    liftIO $ writeChan (sequenceCounterChan chans) seqchan
                    seqn <- liftIO $ readChan seqchan

                    answerchan <- liftIO newChan
                    liftIO $ writeChan (requestsAndResponsesToCenterChan chans) (RegisterSequenceNumber (seqn,answerchan))

                    timestamp <- liftIO getCurrentHTTPTimeStamp

                    let message = defaultValue { Msg.sender = unsafeToUtf8 usr,
                                                 Msg.receiver = unsafeToUtf8 dst,
                                                 Msg.timestamp = unsafeToUtf8 timestamp,
                                                 Msg.body = Just $ unsafeToUtf8 mesg }

                    let request = defaultValue { Rq.sequence_number = fromIntegral seqn,
                                                 Rq.type' = SENDMESSAGE,
                                                 Rq.user_name = Just $ unsafeToUtf8 usr,
                                                 Rq.channel_id = Just $ unsafeToUtf8 channel,
                                                 Rq.mesg = Just message }
                    liftIO $ writeChan (brokerRequestChan chans) request

                    brokeranswer <- liftIO $ readChan answerchan

                    case Rp.type' brokeranswer of
                        SENTMESSAGE -> do
                            let jsonresponse = responseToJSON brokeranswer
                            setHeader "Content-length" (show . BS.length $ jsonresponse)
                            outputFPS jsonresponse
                        _ -> outputError 500 "Sorry, this is an implementation error. [handleSendMessage,wrongAnswerType]" []
        _ -> outputError 400 "Message send request lacking request parameter(s)" []

handleStatusRequest :: ChanInfo -> CGI CGIResult
handleStatusRequest chans = do
    usr_raw <- getInputFPS "user_name"
    case (usr_raw) of
        (Just usr) -> do
                    seqchan <- liftIO $ newChan
                    liftIO $ writeChan (sequenceCounterChan chans) seqchan
                    seqn <- liftIO $ readChan seqchan

                    answerchan <- liftIO newChan
                    liftIO $ writeChan (requestsAndResponsesToCenterChan chans) (RegisterSequenceNumber (seqn,answerchan))

                    let request = defaultValue { Rq.sequence_number = fromIntegral seqn,
                                                 Rq.type' = QUERYSTATUS,
                                                 Rq.user_name = Just $ unsafeToUtf8 usr }
                    liftIO $ writeChan (brokerRequestChan chans) request

                    brokeranswer <- liftIO $ readChan answerchan

                    case Rp.type' brokeranswer of
                        USERSTATUS -> do
                            let jsonresponse = responseToJSON brokeranswer
                            setHeader "Content-length" (show . BS.length $ jsonresponse)
                            outputFPS jsonresponse
                        _ -> outputError 500 "Sorry, this is an implementation error. [handleStatusRequest,wrongAnswerType]" []
        _ -> outputError 400 "Status request lacking request parameter" []

handleMessagesRequest :: ChanInfo -> CGI CGIResult
handleMessagesRequest chans = do
    usr_raw <- getInputFPS "user_name"
    chan_raw <- getInputFPS "channel_id"
    case (usr_raw,chan_raw) of
        (Just usr, Just chan) -> do
                    seqchan <- liftIO newChan
                    liftIO $ writeChan (sequenceCounterChan chans) seqchan
                    seqn <- liftIO $ readChan seqchan

                    answerchan <- liftIO newChan
                    liftIO $ writeChan (requestsAndResponsesToCenterChan chans) (RegisterSequenceNumber (seqn,answerchan))

                    let request = defaultValue { Rq.sequence_number = fromIntegral seqn,
                                                 Rq.type' = GETMESSAGES,
                                                 Rq.user_name = Just $ unsafeToUtf8 usr,
                                                 Rq.channel_id = Just $ unsafeToUtf8 chan }
                    liftIO $ writeChan (brokerRequestChan chans) request

                    brokeranswer <- liftIO $ readChan answerchan

                    case Rp.type' brokeranswer of
                        GOTMESSAGES -> do
                            let jsonresponse = responseToJSON brokeranswer
                            setHeader "Content-length" (show . BS.length $ jsonresponse)
                            outputFPS jsonresponse
                        _ -> outputError 500 "Sorry, this is an implementation error [handleMessagesRequest,wrongAnswerType]" []
        _ -> outputError 400 "Saved-messages request lacking request parameter" []

handleConfSaveRequest :: ChanInfo -> CGI CGIResult
handleConfSaveRequest chans = do
    usr_raw <- getInputFPS "user_name"
    chan_raw <- getInputFPS "channel_id"
    raw_settings <- getBodyFPS
    case (usr_raw,chan_raw) of
        (Just usr, Just chan) -> do
                    seqchan <- liftIO newChan
                    liftIO $ writeChan (sequenceCounterChan chans) seqchan
                    seqn <- liftIO $ readChan seqchan

                    answerchan <- liftIO newChan
                    liftIO $ writeChan (requestsAndResponsesToCenterChan chans) (RegisterSequenceNumber (seqn,answerchan))

                    let request = defaultValue { Rq.sequence_number = fromIntegral seqn,
                                                 Rq.type' = Rq.SAVESETTINGS,
                                                 Rq.user_name = Just $ unsafeToUtf8 usr,
                                                 Rq.channel_id = Just $ unsafeToUtf8 chan,
                                                 Rq.settings = Just $ unsafeToUtf8 raw_settings }

                    liftIO $ writeChan (brokerRequestChan chans) request

                    brokeranswer <- liftIO $ readChan answerchan

                    case Rp.type' brokeranswer of
                        Rp.SAVEDSETTINGS -> do -- default value
                            let jsonresponse = responseToJSON brokeranswer
                            setHeader "Content-length" (show . BS.length $ jsonresponse)
                            outputFPS jsonresponse
                        _ -> outputError 500 "Sorry, this is an implementation error [handleConfSaveRequest,wrongAnswerType]" []
        _ -> outputError 400 "Save-settings request lacking request parameter" []

handleConfGetRequest :: ChanInfo -> CGI CGIResult
handleConfGetRequest chans = do
    usr_raw <- getInputFPS "user_name"
    chan_raw <- getInputFPS "channel_id"
    case (usr_raw,chan_raw) of
        (Just usr, Just chan) -> do
                    seqchan <- liftIO newChan
                    liftIO $ writeChan (sequenceCounterChan chans) seqchan
                    seqn <- liftIO $ readChan seqchan

                    answerchan <- liftIO newChan
                    liftIO $ writeChan (requestsAndResponsesToCenterChan chans) (RegisterSequenceNumber (seqn,answerchan))

                    let request = defaultValue { Rq.sequence_number = fromIntegral seqn,
                                                 Rq.type' = Rq.GETSETTINGS,
                                                 Rq.user_name = Just $ unsafeToUtf8 usr,
                                                 Rq.channel_id = Just $ unsafeToUtf8 chan }

                    liftIO $ writeChan (brokerRequestChan chans) (request)

                    brokeranswer <- liftIO $ readChan answerchan

                    case Rp.type' brokeranswer of
                        Rp.GOTSETTINGS -> do
                                let jsonresponse = responseToJSON brokeranswer
                                setHeader "Content-length" (show . BS.length $ jsonresponse)
                                outputFPS jsonresponse
                        _ -> outputError 500 "Sorry, this is an implementation error [handleConfSaveRequest,wrongAnswerType]" []
        _ -> outputError 400 "Get-settings request lacking request parameter" []

-- Obtain operation (login, logout...) from DOCUMENT_URI

data UrlOp = WebLogin
           | WebLogout
           | WebRegister
           | WebSendMessage
           | WebStatusRequest
           | WebMessagesRequest
           | WebConfSaveRequest
           | WebConfGetRequest
           | VoidRequest String -- malformed request URL
           deriving Show

getOpType :: BS.ByteString -> UrlOp
getOpType url = case parse opParser url of
                    Done _ op -> op
                    Fail _ _ s -> VoidRequest s

opParser :: Parser UrlOp
opParser = do
    char '/'
    manyTill anyChar (string "chattp_request/")
    choice [string "send" >> return WebSendMessage,
            string "isonline" >> return WebStatusRequest,
            string "logout" >> return WebLogout,
            string "register" >> return WebRegister,
            string "login" >> return WebLogin,
            string "savedmessages" >> return WebMessagesRequest,
            string "setconf" >> return WebConfSaveRequest,
            string "getconf" >> return WebConfGetRequest,
            many1 anyChar >>= \rq -> return (VoidRequest rq)]

-- time functions

getCurrentHTTPTimeStamp :: IO BS.ByteString
getCurrentHTTPTimeStamp = do
    t <- getUnixTime
    return . BS.fromStrict $ formatUnixTimeGMT webDateFormat t

