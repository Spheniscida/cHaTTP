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
import Data.Foldable (toList)

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
                -- Create several requests for several channel ids (the channel id sequence in the MessageRelayRequest object is ignored)
                let http_reqs = map (makeHTTPRequest allconf broker_request . utf8) (toList (Rq.channel_id broker_request))
                send_result <- sendFailSafe 3 hstream http_reqs []

                -- Check if sending was successful
                (hstream',response_message) <- case send_result of
                    Just (hstream',http_resps) -> do
                                    -- Create positive response if all requests were answered to with a 20x (x = 0,1,2) code
                                    broker_response <- handleResponse (Rq.type' broker_request) (allResponsesGood http_resps)
                                    return (Just hstream',broker_response)
                    Nothing -> do
                                    broker_response <- handleResponse (Rq.type' broker_request) False
                                    return (Nothing,broker_response)

                _ <- sendTo sock (BS.toStrict . messagePut $ response_message { Rp.sequence_number = Rq.sequence_number broker_request } ) from
                router hstream' allconf
    -- tries to send three times (usually called with count = 0), stops after. For the case that the web server closed the connection
    where   sendFailSafe :: Int -> HandleStream BS.ByteString -> [Request BS.ByteString] -> [Response BS.ByteString] -> IO (Maybe (HandleStream BS.ByteString,[Response BS.ByteString]))
            sendFailSafe _ hstream [] resp = return $ Just (hstream,resp)
            sendFailSafe count hstream (req:reqs) resp
                | count > 0 = do
                    result <- sendHTTP hstream req
                    case result of
                        Right rp -> sendFailSafe count hstream reqs (rp:resp)
                        Left ErrorClosed -> do
                            hstream' <- makeHTTPConnection relayconf
                            sendFailSafe (count-1) hstream' (req:reqs) resp
                        Left _ -> return Nothing
                | otherwise = return Nothing



makeHTTPRequest :: RouterConfig -> MessageRelayRequest -> BS.ByteString -> Request BS.ByteString
--- CREATECHANNEL
makeHTTPRequest (RouterConfig relayconf _) rq@(MessageRelayRequest { Rq.type' = CREATECHANNEL }) chanid =
    -- publishURL is the URL up to the '=' character.
    let url = BS.append (publishURL relayconf) chanid
        empty_val = "" :: Value -- explicit type
        json_data = encode $ makeJSONMessage True defaultValue -- ignore this empty message
        http_req = (postRequest (BS.unpack url)) { rqBody = json_data,
                                                   rqHeaders = makeHTTPHeaders (fromIntegral . BS.length $ json_data) }
       in http_req
--- DELETECHANNEL
makeHTTPRequest (RouterConfig relayconf _) rq@(MessageRelayRequest { Rq.type' = DELETECHANNEL }) chanid =
    let url = BS.append (publishURL relayconf) chanid
        Just u = parseURI (BS.unpack url)
        http_req = Request { rqURI = u,
                             rqMethod = DELETE,
                             rqHeaders = [],
                             rqBody = BS.empty
    }
        in http_req
--- SENDMESSAGE
makeHTTPRequest (RouterConfig relayconf _) rq@(MessageRelayRequest { Rq.type' = SENDMESSAGE }) chanid =
    let url = BS.append (publishURL relayconf) chanid
        json_data = encode $ makeJSONMessage False (fromMaybe defaultValue (Rq.mesg rq))
        http_req = (postRequest (BS.unpack url)) { rqBody = json_data,
                                                   rqHeaders = makeHTTPHeaders (fromIntegral . BS.length $ json_data) }
        in http_req

makeHTTPHeaders :: Int -> [Header]
makeHTTPHeaders length = [Header HdrContentType "application/json",
                          Header HdrConnection "keep-alive",
                          Header HdrContentLength (show length) ]

handleResponse :: MessageRelayRequestType -> Bool -> IO MessageRelayResponse
handleResponse SENDMESSAGE p = return (defaultValue { Rp.status = Just p, Rp.type' = SENTMESSAGE })
handleResponse CREATECHANNEL p = return (defaultValue { Rp.status = Just p, Rp.type' = CREATEDCHANNEL })
handleResponse DELETECHANNEL p = return (defaultValue { Rp.status = Just p, Rp.type' = DELETEDCHANNEL })

allResponsesGood :: [Response BS.ByteString] -> Bool
allResponsesGood = all (\rp -> rspCode rp == (2,0,0) || rspCode rp == (2,0,1) || rspCode rp == (2,0,2))

