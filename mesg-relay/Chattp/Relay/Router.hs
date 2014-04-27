{-# LANGUAGE OverloadedStrings #-}

module Chattp.Relay.Router where

import Chattp.Relay.Config
import Chattp.Relay.Protocol

import qualified Data.Text.Lazy.Encoding as T

import Data.Aeson.Encode
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BS

import Control.Concurrent.MVar

import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import Network.URI
import Network.HTTP

data RouterConfig = RouterConfig {
    relayConfig :: RelayConfig,
    httpStream :: HandleStream BS.ByteString,
    incomingSocket :: Socket,
    httpSync :: MVar ()
}

makeRouterConfig :: RelayConfig -> IO RouterConfig
makeRouterConfig conf = do
    ht_stream <- openTCPConnection (publishHost conf) (publishPort conf)
    undefined
-- This function receives requests, converts them to JSON/HTTP and answers them.

router :: RouterConfig -> IO ()
router allconf@(RouterConfig _ _ sock _) = do
    (msg,from) <- recvFrom sock 16384 -- Max. raw message length of message broker
    case parseRequest (BS.fromStrict msg) of
        Left _ -> router allconf -- ignore, malformed.
        Right (BrokerRequestMessage seqn rq) -> do
                                            resp <- processRequest allconf rq
                                            sendTo sock (BS.toStrict . responseToRaw $ BrokerResponseMessage seqn resp) from
                                            router allconf
    undefined

processRequest :: RouterConfig -> BrokerRequest -> IO BrokerResponse
processRequest (RouterConfig relayconf hstream _ htsync) (NEWCHAN chanid) = do
    -- publishURL is the URL up to the '=' character.
    let url = BS.append (publishURL relayconf) chanid
    let empty_val = "" :: Value -- explicit type
    let json_data = encode . object $ ["ignore" .= True, "message" .= empty_val, "from" .= empty_val]
    let http_req = (postRequest (BS.unpack url)) { rqBody = json_data,
                                                   rqHeaders = [Header HdrContentType "application/json"] }
    takeMVar htsync
    result <- sendHTTP hstream http_req
    putMVar htsync ()
    case result of
        Left _e -> putStrLn "Connection error at HTTP server!" >> return (CHANCREAT FAIL)
        Right resp -> case rspCode resp of
                        (2,0,2) -> return (CHANCREAT OK)
                        c -> putStrLn ("Unexpected HTTP code (chancreat) " ++ show c) >> return (CHANCREAT FAIL)

processRequest (RouterConfig relayconf hstream _ htsync) (DELCHAN chanid) = do
    let url = BS.append (publishURL relayconf) chanid
    case parseURI (BS.unpack url) of
        Nothing -> return (DELTDCHAN FAIL)
        Just u -> do
                let http_req = Request { rqURI = u,
                                         rqMethod = DELETE,
                                         rqHeaders = [],
                                         rqBody = ("" :: BS.ByteString)
                }
                takeMVar htsync
                result <- sendHTTP hstream http_req
                putMVar htsync ()
                case result of
                    Left _e -> putStrLn "Connection error at HTTP server" >> return (DELTDCHAN FAIL)
                    Right resp -> case rspCode resp of
                                    (2,0,0) -> return (DELTDCHAN OK)
                                    c -> putStrLn ("Unexpected HTTP code (delchan) " ++ show c) >> return (CHANCREAT FAIL)
processRequest (RouterConfig relayconf hstream _ htsync) (SNDMSG from chan msg) = do
    let url = BS.append (publishURL relayconf) chan
    let json_data = encode . object $ ["ignore" .= False, "message" .= T.decodeUtf8 msg, "from" .= T.decodeUtf8 from]
    let http_req = (postRequest (BS.unpack url)) { rqBody = json_data,
                                                   rqHeaders = [Header HdrContentType "application/json"] }
    takeMVar htsync
    result <- sendHTTP hstream http_req
    putMVar htsync ()
    case result of
        Left _e -> putStrLn "Connection error at HTTP server!" >> return (MSGSNT FAIL)
        Right resp -> case rspCode resp of
                        (2,0,2) -> return (MSGSNT OK)
                        c -> putStrLn ("Unexpected HTTP code (sndmsg) " ++ show c) >> return (MSGSNT FAIL)

