{-# LANGUAGE OverloadedStrings #-}

module Chattp.Webapp.FastCGI where

import Chattp.Webapp.InternalCommunication
import Chattp.Webapp.Protocol

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Attoparsec.ByteString.Lazy
import Data.Attoparsec.ByteString.Char8 as AP8 hiding (parse, Done, Fail)

import Network.FastCGI
import Control.Concurrent.Chan

-- This function will be run by runFastCGIConcurrent

fcgiMain :: ChanInfo -> CGI CGIResult
fcgiMain channels = do
    params <- getFCGIConf
    let rq_type = getOpType (docUri params)

    case rq_type of
        WebLogin -> handleLogin channels
        WebLogout -> handleLogout channels
        WebRegister -> handleRegister channels
        WebSendMessage -> handleSendMessage channels
        WebStatusRequest -> handleStatusRequest channels
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
                    liftIO $ writeChan (requestsAndResponsesToCenterChan chans) (FCGICenterRequest (seqn,answerchan))

                    let request = BrokerRequestMessage seqn (Login usr pwd)
                    liftIO $ writeChan (brokerRequestChan chans) request

                    brokeranswer <- liftIO $ readChan answerchan

                    let jsonresponse = responseToJSON brokeranswer
                    outputFPS jsonresponse
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
                    liftIO $ writeChan (requestsAndResponsesToCenterChan chans) (FCGICenterRequest (seqn,answerchan))

                    let request = BrokerRequestMessage seqn (Logout usr channel)
                    liftIO $ writeChan (brokerRequestChan chans) request

                    brokeranswer <- liftIO $ readChan answerchan

                    let jsonresponse = responseToJSON brokeranswer
                    outputFPS jsonresponse
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
                    liftIO $ writeChan (requestsAndResponsesToCenterChan chans) (FCGICenterRequest (seqn,answerchan))

                    let request = BrokerRequestMessage seqn (RegisterUser usr pwd)
                    liftIO $ writeChan (brokerRequestChan chans) request

                    brokeranswer <- liftIO $ readChan answerchan

                    let jsonresponse = responseToJSON brokeranswer
                    outputFPS jsonresponse
        _ -> outputError 400 "Register request lacking request parameter(s)" []

handleSendMessage :: ChanInfo -> CGI CGIResult
handleSendMessage chans = do
    usr_raw <- getInputFPS "user_name"
    channel_raw <- getInputFPS "channel_id"
    dest_raw <- getInputFPS "dest_user"
    mesg <- getBodyFPS
    case (usr_raw,dest_raw,channel_raw) of
        (Just usr, Just dst, Just channel) -> do
                    seqchan <- liftIO $ newChan
                    liftIO $ writeChan (sequenceCounterChan chans) seqchan
                    seqn <- liftIO $ readChan seqchan

                    answerchan <- liftIO newChan
                    liftIO $ writeChan (requestsAndResponsesToCenterChan chans) (FCGICenterRequest (seqn,answerchan))

                    let mangled_message = BS.map mangleMsg mesg
                    let request = BrokerRequestMessage seqn (SendMessage usr channel dst mangled_message)
                    liftIO $ writeChan (brokerRequestChan chans) request

                    brokeranswer <- liftIO $ readChan answerchan

                    let jsonresponse = responseToJSON brokeranswer
                    outputFPS jsonresponse
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
                    liftIO $ writeChan (requestsAndResponsesToCenterChan chans) (FCGICenterRequest (seqn,answerchan))

                    let request = BrokerRequestMessage seqn (QueryStatus usr)
                    liftIO $ writeChan (brokerRequestChan chans) request

                    brokeranswer <- liftIO $ readChan answerchan

                    let jsonresponse = responseToJSON brokeranswer
                    outputFPS jsonresponse
        _ -> outputError 400 "Status request lacking request parameter" []

-- Tools.

data FCGIParams = Params { bodyLength :: Int,
                           body :: BS.ByteString,
                           docUri :: BS.ByteString
} deriving Show

getFCGIConf :: CGI FCGIParams
getFCGIConf = do
    content_length_raw <- getVar "CONTENT_LENGTH"
    rq_body <- getBodyFPS
    uri_raw <- getVar "DOCUMENT_URI"
    let content_length = case content_length_raw of
                            Nothing -> 0
                            Just "" -> 0
                            Just l -> read l
    let uri = case uri_raw of
                Nothing -> ""
                Just u -> BS.pack u
    return Params { bodyLength = content_length,
                    body = rq_body,
                    docUri = uri
    }


-- Obtain operation (login, logout...) from DOCUMENT_URI

data UrlOp = WebLogin
           | WebLogout
           | WebRegister
           | WebSendMessage
           | WebStatusRequest
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
            many1 anyChar >>= \rq -> return (VoidRequest rq)]

