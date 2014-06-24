module Chattp.PersistencePg.Config where

import qualified Data.ByteString.Char8 as BS

import System.Directory
import System.Environment (getEnv)
import System.IO.Error (catchIOError)

import Database.PostgreSQL.Simple

import Network.Socket

defaultConnString :: String
defaultConnString = "dbname='chattp' user='chattp' password='chattp_default'"

getConnString :: IO String
getConnString = catchIOError (getEnv "CHATTP_POSTGRES_CONNECTION") (const (return defaultConnString))

pgConnection :: IO Connection
pgConnection = getConnString >>= connectPostgreSQL . BS.pack

makePersistenceSocket :: IO Socket
makePersistenceSocket = do
    raw_fam <- getEnv "CHATTP_PERSISTENCE_LAYER_FAMILY"
    raw_addr <- getEnv "CHATTP_PERSISTENCE_LAYER_ADDR"
    case raw_fam of
        "UNIX" -> do
            catchIOError (removeFile raw_addr) (const $ return ())
            sock <- socket AF_UNIX Datagram defaultProtocol
            bind sock (SockAddrUnix raw_addr)
            return sock
        "INET" -> do
            raw_port <- getEnv "CHATTP_PERSISTENCE_LAYER_PORT"
            (ai:_) <- getAddrInfo (Just $ defaultHints { addrFamily = AF_UNSPEC, addrFlags = [AI_PASSIVE] }) (Just raw_addr) (Just raw_port)

            sock <- socket (addrFamily ai) Datagram defaultProtocol
            bind sock (addrAddress ai)
            return sock
        [] -> ioError $ userError "No address family has been provided."
        _ ->  ioError $ userError "An invalid address family has been provided."

makeBrokerSockAddr :: IO SockAddr
makeBrokerSockAddr = do
    raw_fam <- getEnv "CHATTP_PERSISTENCE_LAYER_FAMILY"
    raw_addr <- getEnv "CHATTP_MSGBROKER_PERSISTENCE_BIND_ADDR"
    case raw_fam of
        "UNIX" -> return $ SockAddrUnix raw_addr
        "INET" -> do
            raw_port <- getEnv "CHATTP_MSGBROKER_PERSISTENCE_BIND_PORT"
            (ai:_) <- getAddrInfo (Just $ defaultHints { addrFamily = AF_UNSPEC }) (Just raw_addr) (Just raw_port)
            return (addrAddress ai)
        [] -> ioError $ userError "No address family has been provided"
        _  -> ioError $ userError "An invalid address family has been provided"

