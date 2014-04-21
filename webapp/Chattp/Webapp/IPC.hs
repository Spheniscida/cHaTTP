module Chattp.Webapp.IPC where

import Chattp.Webapp.Conf

import System.Directory
import Network.Socket

createWebappSocket :: WebappConfiguration -> IO Socket
createWebappSocket conf | bindFamily conf == WAFamily_UNIX = createUnixSocket conf
                        | bindFamily conf == WAFamily_INET = createInetSocket conf

createUnixSocket, createInetSocket :: WebappConfiguration -> IO Socket

createUnixSocket conf = do
    sock <- socket AF_UNIX Datagram defaultProtocol
    removeFile (bindAddress conf)
    bind sock (SockAddrUnix (bindAddress conf))
    return sock

createInetSocket conf = do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [],
                                    addrFamily = AF_UNSPEC,
                                    addrSocketType = Datagram }) ) 
                            (Just (bindAddress conf))
                            (Just (bindPort conf))
    if addrinfos == []
     then fail "Couldn't obtain address information (getaddrinfo failed)"
     else do
        let ai = head addrinfos
        sock <- socket (addrFamily ai) Datagram defaultProtocol
        bind sock (addrAddress ai)
        return sock

