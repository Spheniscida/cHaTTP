{-# LANGUAGE OverloadedStrings #-}

module Chattp.Webapp.IPC where

import Chattp.Webapp.Conf
import Chattp.Webapp.Protocol
import Chattp.Webapp.InternalCommunication

import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.WireMessage
import Chattp.WebappRequestMessage as Rq
import Chattp.WebappResponseMessage as Rp

import Control.Concurrent
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Directory
import System.IO.Error

import Network.Socket
import qualified Network.Socket.ByteString as NBS

-- Thread code handling incoming messages
-- This thread parses the messages before sending them to "center"; we will have several threads so there's no
-- performance problem.
socketIncoming :: Socket -> Chan CenterRequestOrResponse -> IO ()
socketIncoming sock chanToCenter = do
    (contents,_addr) <- NBS.recvFrom sock 16384
    case parseAnswer (BS.fromStrict contents) of
        Right msg -> writeChan chanToCenter (BrokerCenterResponse msg) >> socketIncoming sock chanToCenter
        Left err -> putStrLn ("ERR : Discarded message because of: " ++ err) >> socketIncoming sock chanToCenter -- discard

-- Thread code handling outgoing messages
socketOutgoing :: WebappConfiguration -> Socket -> ChanInfo -> IO ()
socketOutgoing conf sock chans = do
    msg <- readChan (brokerRequestChan chans)
    let rawMessage = BS.toStrict . messagePut $ msg
    catchIOError (NBS.sendTo sock rawMessage (brokerSockAddr conf)) (errHandler (requestsAndResponsesToCenterChan chans) msg)
    socketOutgoing conf sock chans

    where errHandler :: Chan CenterRequestOrResponse -> WebappRequestMessage -> IOError -> IO Int -- if there is an error, an error message is sent back.
          errHandler bc msg _ = writeChan bc
            (BrokerCenterResponse $ defaultValue { Rp.sequence_number = Rq.sequence_number msg,
                                                   Rp.status = Just False,
                                                   Rp.error_message = Just $ uFromString "Couldn't reach broker",
                                                   Rp.error_code = Just $ fromIntegral 16 } ) >> return 0

-- Socket setup

createWebappSocket :: WebappConfiguration -> IO Socket
createWebappSocket conf | bindFamily conf == WAFamilyUnix = createUnixSocket conf
                        | bindFamily conf == WAFamilyInet = createInetSocket conf

createUnixSocket, createInetSocket :: WebappConfiguration -> IO Socket

createUnixSocket conf = do
    sock <- socket AF_UNIX Datagram defaultProtocol
    catchIOError (removeFile (bindAddress conf)) (const $ return ())
    bind sock (SockAddrUnix (bindAddress conf))
    return sock

createInetSocket conf = do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [],
                                    addrFamily = AF_UNSPEC,
                                    addrSocketType = Datagram }) )
                            (Just (bindAddress conf))
                            (Just (show $ bindPort conf))
    if null addrinfos
     then fail "Couldn't obtain address information (getaddrinfo failed)"
     else do
        let ai = head addrinfos
        sock <- socket (addrFamily ai) Datagram defaultProtocol
        bind sock (addrAddress ai)
        return sock

