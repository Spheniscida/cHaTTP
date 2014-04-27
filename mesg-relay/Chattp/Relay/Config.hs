module Chattp.Relay.Config where

import Data.Maybe (fromMaybe)
import System.Environment (getEnv, lookupEnv)

data RelayConfig = RelayConfig {
    publishHost :: String,
    publishPort :: String,
    publishBasePath :: String,
    publishChanIdParam :: String,
    nThreads :: Int
} deriving Show

publish_host_env_var, publish_port_env_var, publish_base_path_env_var, publish_chan_id_env_var, nthreads_env_var :: String
publish_host_env_var = "CHATTP_MSGRELAY_PUBLISH_HOST"
publish_port_env_var = "CHATTP_MSGRELAY_PUBLISH_PORT"
publish_base_path_env_var = "CHATTP_MSGRELAY_PUBLISH_BASE_PATH"
publish_chan_id_env_var = "CHATTP_MSGRELAY_PUBLISH_CHAN_ID_PARAMETER"
nthreads_env_var = "CHATTP_MSGRELAY_NUMBER_THREADS"

makeConfig :: IO RelayConfig
makeConfig = do
    p_host <- getEnv publish_host_env_var
    p_port_raw <- lookupEnv publish_port_env_var
    p_base <- getEnv publish_base_path_env_var
    p_chanid <- getEnv publish_chan_id_env_var
    nthreads_raw <- lookupEnv nthreads_env_var
    let p_port = fromMaybe "80" p_port_raw
    let nthreads = maybe 2 read nthreads_raw
    return RelayConfig { publishHost = p_host,
                           publishPort = p_port,
                           publishBasePath = p_base,
                           publishChanIdParam = p_chanid,
                           nThreads = nthreads
    }



