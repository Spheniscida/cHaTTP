module Chattp.Persistence.Interface
( SockAddr
, Socket
, initSocket
, talk
) where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString (recvFrom, sendAllTo)


initSocket :: Family -> SockAddr -> IO Socket
initSocket fam addr = do
    sock <- socket fam Datagram defaultProtocol
    bind sock addr
    return sock

talk :: ([ByteString] -> IO [ByteString]) -> Socket -> IO ()
talk f sock = do
    (rawCmd, remote) <- recvFrom sock maxLen
    rawAnswer <- talk' f rawCmd
    sendAllTo sock rawAnswer remote
        where maxLen = 4096

talk' :: ([ByteString] -> IO [ByteString]) -> ByteString -> IO ByteString
talk' f rawCmd = B.unlines . (seq :) <$> f cmd
    where (seq:cmd) = B.lines rawCmd
