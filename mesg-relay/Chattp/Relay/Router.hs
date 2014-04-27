module Chattp.Relay.Router where

import Chattp.Relay.Config
import Chattp.Relay.Protocol

import Data.Aeson.Encode
import Data.Aeson.Types

import Data.ByteString.Lazy.Char8 as BS
import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import Network.HTTP

data RouterConfig = RouterConfig {
    relayConfig :: RelayConfig,
    httpStream :: HandleStream BS.ByteString,
    incomingSocket :: Socket
}

-- This function receives requests, converts them to JSON/HTTP and answers them.

router :: RouterConfig -> IO ()
router allconf@(RouterConfig relayconf hstream sock) = do
    (msg,from) <- recvFrom sock 16384 -- Max. raw message length of message broker
    case parseRequest (BS.fromStrict msg) of
        Left e -> router allconf -- ignore, malformed.
        Right (BrokerRequestMessage seqn rq) -> do
                                            resp <- processRequest hstream rq
                                            sendTo sock (BS.toStrict . responseToRaw $ BrokerResponseMessage seqn resp) from
                                            router allconf
    undefined

processRequest :: HandleStream BS.ByteString -> BrokerRequest -> IO BrokerResponse
processRequest hstream _ = undefined

