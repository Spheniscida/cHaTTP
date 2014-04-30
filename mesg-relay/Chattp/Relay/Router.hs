{-# LANGUAGE OverloadedStrings #-}

module Chattp.Relay.Router where

import System.Directory
import System.IO.Error
import System.IO

import Chattp.Relay.Config
import Chattp.Relay.Protocol

import qualified Data.Text.Lazy.Encoding as T

import Data.Aeson.Encode
import Data.Aeson.Types hiding (Result)
import qualified Data.ByteString.Lazy.Char8 as BS

import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import Network.URI
import Network.HTTP
import Network.Stream

data RouterConfig = RouterConfig {
    relayConfig :: RelayConfig,
    incomingSocket :: Socket
}

makeHTTPConnection :: RelayConfig -> IO (HandleStream BS.ByteString)
makeHTTPConnection conf = openTCPConnection (publishHost conf) (publishPort conf)

-- Create socket for requests from the broker
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
        Right (BrokerRequestMessage seqn broker_request) -> do
                                            let http_req = processRequest allconf broker_request
                                            send_result <- sendFailSafe 0 hstream http_req
                                            (hstream',response_message) <- case send_result of
                                                Just (hstream',http_resp) -> do
                                                                broker_response <- handleResponse broker_request (Just http_resp)
                                                                return (Just hstream',broker_response)
                                                Nothing -> do
                                                                broker_response <- handleResponse broker_request Nothing
                                                                return (Nothing,broker_response)
                                            _ <- sendTo sock (BS.toStrict . responseToRaw $ BrokerResponseMessage seqn response_message) from
                                            router hstream' allconf
    -- tries to send three times (usually called with count = 0), stops after. For the case that the web server closed the connection
    where   sendFailSafe :: Int -> HandleStream BS.ByteString -> Request BS.ByteString -> IO (Maybe (HandleStream BS.ByteString,Response BS.ByteString))
            sendFailSafe count hstream req | count < 3 = do
                                        result <- sendHTTP hstream req
                                        case result of
                                            Right rp -> return $ Just (hstream,rp)
                                            Left ErrorClosed -> do
                                                hstream' <- makeHTTPConnection relayconf
                                                sendFailSafe (count+1) hstream' req
                                            Left _ -> return Nothing
                                        | otherwise = return Nothing

processRequest :: RouterConfig -> BrokerRequest -> Request BS.ByteString
processRequest (RouterConfig relayconf _) (NEWCHAN chanid) =
    -- publishURL is the URL up to the '=' character.
    let url = BS.append (publishURL relayconf) chanid
        empty_val = "" :: Value -- explicit type
        json_data = encode . object $ ["ignore" .= True, "message" .= empty_val, "from" .= empty_val]
        http_req = (postRequest (BS.unpack url)) { rqBody = json_data,
                                                   rqHeaders = [Header HdrContentType "application/json",
                                                                Header HdrConnection "keep-alive",
                                                                Header HdrContentLength (show . BS.length $ json_data)] }
       in http_req
processRequest (RouterConfig relayconf _) (DELCHAN chanid) = do
    let url = BS.append (publishURL relayconf) chanid
        Just u = parseURI (BS.unpack url)
        http_req = Request { rqURI = u,
                             rqMethod = DELETE,
                             rqHeaders = [],
                             rqBody = "" :: BS.ByteString
    }
        in http_req
processRequest (RouterConfig relayconf _) (SNDMSG from chan msg) = do
    let url = BS.append (publishURL relayconf) chan
        json_data = encode . object $ ["ignore" .= False, "message" .= T.decodeUtf8 msg, "from" .= T.decodeUtf8 from]
        http_req = (postRequest (BS.unpack url)) { rqBody = json_data,
                                                   rqHeaders = [Header HdrContentType "application/json",
                                                                Header HdrConnection "keep-alive",
                                                                Header HdrContentLength (show . BS.length $ json_data)] }
        in http_req


handleResponse :: BrokerRequest -> Maybe (Response (BS.ByteString)) -> IO BrokerResponse
handleResponse (SNDMSG _ _ _) Nothing = return (MSGSNT FAIL)
handleResponse (SNDMSG _ _ _) (Just resp) =
        case rspCode resp of
                        (2,0,2) -> return (MSGSNT OK)
                        c -> putStrLn ("Unexpected HTTP code (sndmsg) " ++ show c) >> return (MSGSNT FAIL)
handleResponse (NEWCHAN _) Nothing = return (CHANCREAT FAIL)
handleResponse (NEWCHAN _) (Just resp) =
        case rspCode resp of
                        (2,0,2) -> return (CHANCREAT OK)
                        c -> putStrLn ("Unexpected HTTP code (chancreat) " ++ show c) >> return (CHANCREAT FAIL)
handleResponse (DELCHAN _) Nothing = return (DELTDCHAN FAIL)
handleResponse (DELCHAN _) (Just resp) =
        case rspCode resp of
                        (2,0,0) -> return (DELTDCHAN OK)
                        c -> putStrLn ("Unexpected HTTP code (delchan) " ++ show c) >> return (DELTDCHAN FAIL)

