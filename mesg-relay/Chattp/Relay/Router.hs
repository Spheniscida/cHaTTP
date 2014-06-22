{-# LANGUAGE OverloadedStrings #-}

module Chattp.Relay.Router where

import System.Directory
import System.IO.Error

import Chattp.Relay.Config
import Chattp.Relay.Protocol

import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.WireMessage (messagePut)

import Chattp.MessageRelayRequest as Rq
import Chattp.MessageRelayResponse as Rp
import Chattp.MessageRelayRequest.MessageRelayRequestType
import Chattp.MessageRelayResponse.MessageRelayResponseType

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
        Right broker_request -> do
                let http_req = makeHTTPRequest allconf broker_request
                send_result <- sendFailSafe 3 hstream http_req
                (hstream',response_message) <- case send_result of
                    Just (hstream',http_resp) -> do
                                    broker_response <- handleResponse (Rq.type' broker_request) (Just http_resp)
                                    return (Just hstream',broker_response)
                    Nothing -> do
                                    broker_response <- handleResponse (Rq.type' broker_request) Nothing
                                    return (Nothing,broker_response)
                _ <- sendTo sock (BS.toStrict . messagePut $ response_message { Rp.sequence_number = Rq.sequence_number broker_request } ) from
                router hstream' allconf
    -- tries to send three times (usually called with count = 0), stops after. For the case that the web server closed the connection
    where   sendFailSafe :: Int -> HandleStream BS.ByteString -> Request BS.ByteString -> IO (Maybe (HandleStream BS.ByteString,Response BS.ByteString))
            sendFailSafe count hstream req
                | count > 0 = do
                    result <- sendHTTP hstream req
                    case result of
                        Right rp -> return $ Just (hstream,rp)
                        Left ErrorClosed -> do
                            hstream' <- makeHTTPConnection relayconf
                            sendFailSafe (count-1) hstream' req
                        Left _ -> return Nothing
                | otherwise = return Nothing

makeHTTPRequest :: RouterConfig -> MessageRelayRequest -> Request BS.ByteString
--- CREATECHANNEL
makeHTTPRequest (RouterConfig relayconf _) rq@(MessageRelayRequest { Rq.type' = CREATECHANNEL }) =
    -- publishURL is the URL up to the '=' character.
    let url = BS.append (publishURL relayconf) (utf8 $ Rq.channel_id rq)
        empty_val = "" :: Value -- explicit type
        json_data = encode $ makeJSONMessage True defaultValue -- ignore this empty message
        http_req = (postRequest (BS.unpack url)) { rqBody = json_data,
                                                   rqHeaders = makeHTTPHeaders (fromIntegral . BS.length $ json_data) }
       in http_req
--- DELETECHANNEL
makeHTTPRequest (RouterConfig relayconf _) rq@(MessageRelayRequest { Rq.type' = DELETECHANNEL }) =
    let url = BS.append (publishURL relayconf) (utf8 $ Rq.channel_id rq)
        Just u = parseURI (BS.unpack url)
        http_req = Request { rqURI = u,
                             rqMethod = DELETE,
                             rqHeaders = [],
                             rqBody = BS.empty
    }
        in http_req
--- SENDMESSAGE
makeHTTPRequest (RouterConfig relayconf _) rq@(MessageRelayRequest { Rq.type' = SENDMESSAGE }) =
    let url = BS.append (publishURL relayconf) (utf8 $ Rq.channel_id rq)
        json_data = encode $ makeJSONMessage False (fromMaybe defaultValue (Rq.mesg rq))
        http_req = (postRequest (BS.unpack url)) { rqBody = json_data,
                                                   rqHeaders = makeHTTPHeaders (fromIntegral . BS.length $ json_data) }
        in http_req

makeHTTPHeaders :: Int -> [Header]
makeHTTPHeaders length = [Header HdrContentType "application/json",
                          Header HdrConnection "keep-alive",
                          Header HdrContentLength (show length) ]

handleResponse :: MessageRelayRequestType -> Maybe (Response BS.ByteString) -> IO MessageRelayResponse
handleResponse SENDMESSAGE Nothing = return (defaultValue { Rp.status = Just False, Rp.type' = SENTMESSAGE })
handleResponse SENDMESSAGE (Just resp) =
        case rspCode resp of
                        (2,0,x) | x == 1 || x == 2-> return (defaultValue { Rp.status = Just True, Rp.type' = SENTMESSAGE }) -- Delivered or queued.
                        c -> putStrLn ("Unexpected HTTP code (sndmsg) " ++ show c) >> return (defaultValue { Rp.status = Just False, Rp.type' = SENTMESSAGE })
handleResponse CREATECHANNEL Nothing = return (defaultValue { Rp.status = Just False, Rp.type' = CREATEDCHANNEL })
handleResponse CREATECHANNEL (Just resp) =
        case rspCode resp of
                        (2,0,2) -> return (defaultValue { Rp.status = Just True, Rp.type' = CREATEDCHANNEL} )
                        c -> putStrLn ("Unexpected HTTP code (chancreat) " ++ show c) >> return (defaultValue { Rp.status = Just False, Rp.type' = CREATEDCHANNEL })
handleResponse DELETECHANNEL Nothing = return (defaultValue { Rp.status = Just False, Rp.type' = DELETEDCHANNEL })
handleResponse DELETECHANNEL (Just resp) =
        case rspCode resp of
                        (2,0,0) -> return (defaultValue { Rp.status = Just True, Rp.type' = DELETEDCHANNEL })
                        c -> putStrLn ("Unexpected HTTP code (delchan) " ++ show c) >> return (defaultValue { Rp.status = Just False, Rp.type' = DELETEDCHANNEL })

