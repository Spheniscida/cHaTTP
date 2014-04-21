module Chattp.Webapp.Conf where

import System.Environment
import System.IO
import Data.Char

import Control.Exception.Base

addressEnvVar, portEnvVar, familyEnvVar, brokerAddressVar, brokerPortVar :: String
addressEnvVar = "CHATTP_WEBAPP_ADDR"
familyEnvVar  = "CHATTP_WEBAPP_FAMILY"
portEnvVar    = "CHATTP_WEBAPP_PORT"

brokerAddressVar = "CHATTP_MSGBROKER_WEBAPP_BIND_ADDR"
brokerPortVar    = "CHATTP_MSGBROKER_WEBAPP_BIND_PORT"

data WebappAddressFamily = WAFamily_UNIX | WAFamily_INET deriving (Eq, Show)

data WebappConfiguration = WAConfig {
                        bindAddress :: String,
                        bindFamily :: WebappAddressFamily,
                        bindPort :: Maybe Int,
                        brokerAddress :: String,
                        brokerPort :: Maybe Int } deriving (Show)

getConfig :: IO WebappConfiguration
getConfig = do
    family_raw <- getEnv familyEnvVar
    our_address <- getEnv addressEnvVar
    our_port_raw <- getEnv portEnvVar
    broker_address <- getEnv brokerAddressVar
    broker_port_raw <- getEnv brokerPortVar

    family <- case parseFamily family_raw of
                    Just fam -> return fam
                    Nothing -> fail "Unknown address family"
    let our_port = if family == WAFamily_UNIX then Nothing else parsePort our_port_raw
    let broker_port = if family == WAFamily_UNIX then Nothing else parsePort our_port_raw
    let config = WAConfig { bindFamily = family, bindAddress = our_address, bindPort = our_port, brokerAddress = broker_address, brokerPort = broker_port }
    if checkConfig config then return config else fail "Environment configuration error; aborting (hint: use the original env_vars script)"

parseFamily :: String -> Maybe WebappAddressFamily
parseFamily "UNIX" = Just WAFamily_UNIX
parseFamily "INET" = Just WAFamily_INET
parseFamily _      = Nothing

parsePort :: String -> Maybe Int
parsePort s | length s == 0 = Nothing
            | all isNumber s = Just $ read s
parsePort _ = Nothing

checkConfig :: WebappConfiguration -> Bool
checkConfig conf =
       (length (bindAddress conf) > 0)
    && (length (brokerAddress conf) > 0)
    && (bindFamily conf == WAFamily_UNIX
            || ( -- If family is INET, then the ports must be filled
                   bindFamily conf == WAFamily_INET
                && bindPort conf /= Nothing
                && brokerPort conf /= Nothing
               )
       )
