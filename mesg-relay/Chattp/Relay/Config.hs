module Chattp.Relay.Config where

import System.Environment (getEnv, lookupEnv)

import qualified Data.ByteString.Lazy.Char8 as BS

data Family = Inet | Unix deriving (Show,Eq)

data RelayConfig = RelayConfig {
    publishHost :: String,
    publishPort :: Int,
    publishBasePath :: String,
    publishChanIdParam :: String,
    myHost :: String,
    myFamily :: Family,
    myPort :: Int,
    brokerHost :: String,
    brokerPort :: Int,
    nThreads :: Int,
    publishURL :: BS.ByteString
} deriving Show

publish_host_env_var, publish_port_env_var, publish_base_path_env_var :: String
publish_chan_id_env_var, broker_addr_env_var, broker_port_env_var, nthreads_env_var :: String
self_bind_addr_env_var, self_bind_family_env_var, self_bind_port_env_var :: String

publish_host_env_var = "CHATTP_MSGRELAY_PUBLISH_HOST"
publish_port_env_var = "CHATTP_MSGRELAY_PUBLISH_PORT"
publish_base_path_env_var = "CHATTP_MSGRELAY_PUBLISH_BASE_PATH"
publish_chan_id_env_var = "CHATTP_MSGRELAY_PUBLISH_CHAN_ID_PARAMETER"
broker_addr_env_var = "CHATTP_MSGBROKER_MSGRELAY_BIND_ADDR"
broker_port_env_var = "CHATTP_MSGBROKER_MSGRELAY_BIND_PORT"
self_bind_addr_env_var = "CHATTP_MESG_RELAY_ADDR"
self_bind_family_env_var = "CHATTP_MESG_RELAY_FAMILY"
self_bind_port_env_var = "CHATTP_MESG_RELAY_PORT"
nthreads_env_var = "CHATTP_MSGRELAY_NUMBER_THREADS"

makeConfig :: IO RelayConfig
makeConfig = do
    -- web server info
    p_host <- getEnv publish_host_env_var
    p_port_raw <- lookupEnv publish_port_env_var
    p_base <- getEnv publish_base_path_env_var
    p_chanid <- getEnv publish_chan_id_env_var
    let p_port = maybe 80 read p_port_raw
    -- self bind info
    my_family_raw <- getEnv self_bind_family_env_var
    let my_family = case my_family_raw of
                    "UNIX" -> Unix
                    "INET" -> Inet
    my_addr <- getEnv self_bind_addr_env_var
    my_port <- if my_family == Inet then fmap read (getEnv self_bind_port_env_var) else return 0
    -- broker info
    broker_addr <- getEnv broker_addr_env_var
    broker_port <- if my_family == Inet then fmap read (getEnv broker_port_env_var) else return 0
    --
    nthreads_raw <- lookupEnv nthreads_env_var
    let nthreads = maybe 2 read nthreads_raw

    return RelayConfig { publishHost = p_host,
                           publishPort = p_port,
                           publishBasePath = p_base,
                           publishChanIdParam = p_chanid,
                           myHost = my_addr,
                           myFamily = my_family,
                           myPort = my_port,
                           brokerHost = broker_addr,
                           brokerPort = broker_port,
                           nThreads = nthreads,
                           publishURL = BS.pack $ "http://" ++ p_host ++ p_base ++ "/pub?" ++ p_chanid ++ "="

    }

