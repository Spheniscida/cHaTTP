{-# LANGUAGE OverloadedStrings #-}

module Chattp.Webapp.FastCGI where

import Chattp.Webapp.InternalCommunication
import Chattp.Webapp.Protocol
import Chattp.Webapp.Storage

import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Encoding as TS

import Data.Aeson as AE

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Attoparsec.ByteString.Lazy
import Data.Attoparsec.ByteString.Char8 as AP8 hiding (parse, Done, Fail)

import Network.FastCGI
import Control.Concurrent.Chan

import Database.Redis

-- This function will be run by runFastCGIConcurrent

fcgiMain :: Connection -> ChanInfo -> CGI CGIResult
fcgiMain r_conn channels = do
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
        WebConfSaveRequest -> handleConfSaveRequest r_conn channels
        WebConfGetRequest -> handleConfGetRequest r_conn channels
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

                    let request = BrokerRequestMessage seqn (Login usr pwd)
                    liftIO $ writeChan (brokerRequestChan chans) (request)

                    brokeranswer <- liftIO $ readChan answerchan

                    case brokeranswer of
                        (UserLoggedIn _ _) -> do
                            let jsonresponse = responseToJSON brokeranswer
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

                    let request = BrokerRequestMessage seqn (Logout usr channel)
                    liftIO $ writeChan (brokerRequestChan chans) (request)

                    brokeranswer <- liftIO $ readChan answerchan

                    case brokeranswer of
                        (UserLoggedOut _) -> do
                            let jsonresponse = responseToJSON brokeranswer
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

                    let request = BrokerRequestMessage seqn (RegisterUser usr pwd)
                    liftIO $ writeChan (brokerRequestChan chans) (request)

                    brokeranswer <- liftIO $ readChan answerchan

                    case brokeranswer of
                        (UserRegistered _) -> do
                            let jsonresponse = responseToJSON brokeranswer
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

                    let mangled_message = BS.map mangleMsg mesg
                    let request = BrokerRequestMessage seqn (SendMessage usr channel dst mangled_message)
                    liftIO $ writeChan (brokerRequestChan chans) (request)

                    brokeranswer <- liftIO $ readChan answerchan

                    case brokeranswer of
                        (MessageAccepted _) -> do
                            let jsonresponse = responseToJSON brokeranswer
                            outputFPS jsonresponse
                        _ -> outputError 500 "Sorry, this is an implementation error. [handleSendMessage,wrongAnswerType]" []
        _ -> outputError 400 "Message send request lacking request parameter(s)" []
    where mangleMsg :: Char -> Char
          mangleMsg '\n' = ' '
          mangleMsg c = c

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

                    let request = BrokerRequestMessage seqn (QueryStatus usr)
                    liftIO $ writeChan (brokerRequestChan chans) (request)

                    brokeranswer <- liftIO $ readChan answerchan

                    case brokeranswer of
                        (UserStatus _) -> do
                            let jsonresponse = responseToJSON brokeranswer
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

                    let request = BrokerRequestMessage seqn (GetMessages usr chan)
                    liftIO $ writeChan (brokerRequestChan chans) (request)

                    brokeranswer <- liftIO $ readChan answerchan

                    case brokeranswer of
                        (SavedMessages _ _) -> do
                            let jsonresponse = responseToJSON brokeranswer
                            outputFPS jsonresponse
                        _ -> outputError 500 "Sorry, this is an implementation error [handleMessagesRequest,wrongAnswerType]" []
        _ -> outputError 400 "Saved-messages request lacking request parameter" []

handleConfSaveRequest :: Connection -> ChanInfo -> CGI CGIResult
handleConfSaveRequest conn chans = do
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

                    let request = BrokerRequestMessage seqn (IsAuthorized usr chan)
                    liftIO $ writeChan (brokerRequestChan chans) (request)

                    brokeranswer <- liftIO $ readChan answerchan

                    case brokeranswer of
                        Authorized True -> do
                                result <- liftIO $ storeUserSettings conn usr raw_settings

                                let (status,err) = if result == ""
                                                   then (True,"")
                                                   else (False,result)

                                outputFPS . encode $ object ["type" .= T.decodeUtf8 "saved-settings",
                                                          "status" .= status,
                                                          "error" .= (T.decodeUtf8 . BS.pack $ err)]
                        Authorized False -> outputFPS $ responseToJSON brokeranswer
                        _ -> outputError 500 "Sorry, this is an implementation error [handleConfSaveRequest,wrongAnswerType]" []
        _ -> outputError 400 "Save-settings request lacking request parameter" []

handleConfGetRequest :: Connection -> ChanInfo -> CGI CGIResult
handleConfGetRequest conn chans = do
    usr_raw <- getInputFPS "user_name"
    chan_raw <- getInputFPS "channel_id"
    case (usr_raw,chan_raw) of
        (Just usr, Just chan) -> do
                    seqchan <- liftIO newChan
                    liftIO $ writeChan (sequenceCounterChan chans) seqchan
                    seqn <- liftIO $ readChan seqchan

                    answerchan <- liftIO newChan
                    liftIO $ writeChan (requestsAndResponsesToCenterChan chans) (RegisterSequenceNumber (seqn,answerchan))

                    let request = BrokerRequestMessage seqn (IsAuthorized usr chan)
                    liftIO $ writeChan (brokerRequestChan chans) (request)

                    brokeranswer <- liftIO $ readChan answerchan

                    case brokeranswer of
                        Authorized True -> do
                                settings <- liftIO $ getUserSettings conn usr
                                let settings_json = case AE.decode settings of
                                                        Just o -> o
                                                        Nothing -> AE.String . TS.decodeUtf8 . BS.toStrict $ settings

                                outputFPS . encode $ object ["type" .= T.decodeUtf8 "saved-settings",
                                                          "status" .= True,
                                                          "error" .= T.decodeUtf8 "",
                                                          "settings" .= settings_json]
                        Authorized False -> outputFPS $ responseToJSON brokeranswer
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

