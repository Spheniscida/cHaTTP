{-# LANGUAGE OverloadedStrings #-}

module Chattp.Relay.Router where

import System.Directory
import System.IO.Error

import Chattp.Relay.Config
import Chattp.Relay.Protocol

import qualified Data.Text.Lazy.Encoding as T

import Data.Aeson.Encode
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BS

import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import Network.URI
import Network.HTTP

data RouterConfig = RouterConfig {
    relayConfig :: RelayConfig,
    incomingSocket :: Socket
}

makeHTTPConnection :: RelayConfig -> IO (HandleStream BS.ByteString)
makeHTTPConnection conf = openTCPConnection (publishHost conf) (publishPort conf)

makeRouterConfig :: RelayConfig -> IO RouterConfig
makeRouterConfig conf = do
    sock <- case myFamily conf of
        Unix -> do
                sock <- socket AF_UNIX Datagram defaultProtocol
                catchIOError (removeFile (myHost conf)) (const $ return ())
                bind sock (SockAddrUnix (myHost conf))
                return sock
        Inet -> do
            ais <- getAddrInfo (Just $ defaultHints { addrSocketType = Datagram, addrFamily = AF_UNSPEC }) (Just $ myHost conf) (Just . show $ myPort conf)
            case ais of
                [] -> fail "Couldn't get address information for my socket"
                (ai:_) -> do
                 sock <- socket (addrFamily ai) Datagram defaultProtocol
                 bind sock (addrAddress ai)
                 return sock
    return RouterConfig { relayConfig = conf, incomingSocket = sock }

-- This function receives requests, converts them to JSON/HTTP and answers them.

router :: Maybe (HandleStream BS.ByteString) -> RouterConfig -> IO ()
router hs allconf@(RouterConfig relayconf sock) = do
    hstream <- case hs of
                    Nothing -> makeHTTPConnection relayconf
                    Just x -> return x
    (msg,from) <- recvFrom sock 16384 -- Max. raw message length of message broker
    case parseRequest (BS.fromStrict msg) of
        Left _ -> router (Just hstream) allconf -- ignore, malformed.
        Right (BrokerRequestMessage seqn rq) -> do
                                            connstate <- isTCPConnectedTo hstream EndPoint{ epHost = publishHost relayconf, epPort = publishPort relayconf }
                                            hstream' <- if connstate
                                                         then return hstream
                                                         else makeHTTPConnection relayconf
                                            resp <- processRequest hstream allconf rq
                                            _ <- sendTo sock (BS.toStrict . responseToRaw $ BrokerResponseMessage seqn resp) from
                                            router (Just hstream') allconf

processRequest :: HandleStream BS.ByteString -> RouterConfig -> BrokerRequest -> IO BrokerResponse
processRequest hstream (RouterConfig relayconf _) (NEWCHAN chanid) = do
    -- publishURL is the URL up to the '=' character.
    let url = BS.append (publishURL relayconf) chanid
    let empty_val = "" :: Value -- explicit type
    let json_data = encode . object $ ["ignore" .= True, "message" .= empty_val, "from" .= empty_val]
    let http_req = (postRequest (BS.unpack url)) { rqBody = json_data,
                                                   rqHeaders = [Header HdrContentType "application/json",
                                                                Header HdrConnection "keep-alive",
                                                                Header HdrContentLength (show . BS.length $ json_data)] }
    result <- sendHTTP hstream http_req
    case result of
        Left _e -> putStrLn "Connection error at HTTP server!" >> return (CHANCREAT FAIL)
        Right resp -> case rspCode resp of
                        (2,0,2) -> return (CHANCREAT OK)
                        c -> putStrLn ("Unexpected HTTP code (chancreat) " ++ show c) >> return (CHANCREAT FAIL)

processRequest hstream (RouterConfig relayconf _) (DELCHAN chanid) = do
    let url = BS.append (publishURL relayconf) chanid
    case parseURI (BS.unpack url) of
        Nothing -> return (DELTDCHAN FAIL)
        Just u -> do
                let http_req = Request { rqURI = u,
                                         rqMethod = DELETE,
                                         rqHeaders = [],
                                         rqBody = "" :: BS.ByteString
                }
                result <- sendHTTP hstream http_req
                case result of
                    Left _e -> putStrLn "Connection error at HTTP server" >> return (DELTDCHAN FAIL)
                    Right resp -> case rspCode resp of
                                    (2,0,0) -> return (DELTDCHAN OK)
                                    c -> putStrLn ("Unexpected HTTP code (delchan) " ++ show c) >> return (CHANCREAT FAIL)
processRequest hstream (RouterConfig relayconf _) (SNDMSG from chan msg) = do
    let url = BS.append (publishURL relayconf) chan
    let json_data = encode . object $ ["ignore" .= False, "message" .= T.decodeUtf8 msg, "from" .= T.decodeUtf8 from]
    let http_req = (postRequest (BS.unpack url)) { rqBody = json_data,
                                                   rqHeaders = [Header HdrContentType "application/json",
                                                                Header HdrConnection "keep-alive",
                                                                Header HdrContentLength (show . BS.length $ json_data)] }
    result <- sendHTTP hstream http_req
    case result of
        Left _e -> putStrLn "Connection error at HTTP server!" >> return (MSGSNT FAIL)
        Right resp -> case rspCode resp of
                        (2,0,2) -> return (MSGSNT OK)
                        c -> putStrLn ("Unexpected HTTP code (sndmsg) " ++ show c) >> return (MSGSNT FAIL)

