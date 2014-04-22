module Chattp.Webapp.Conf where

import Network.Socket
import System.Environment
import Data.Char

addressEnvVar, portEnvVar, familyEnvVar, brokerAddressVar, brokerPortVar :: String
addressEnvVar = "CHATTP_WEBAPP_ADDR"
familyEnvVar  = "CHATTP_WEBAPP_FAMILY"
portEnvVar    = "CHATTP_WEBAPP_PORT"

brokerAddressVar = "CHATTP_MSGBROKER_WEBAPP_BIND_ADDR"
brokerPortVar    = "CHATTP_MSGBROKER_WEBAPP_BIND_PORT"

data WebappAddressFamily = WAFamilyUnix | WAFamilyInet deriving (Eq, Show)

data WebappConfiguration = WAConfig {
                        bindAddress :: String,
                        bindFamily :: WebappAddressFamily,
                        bindPort :: Int,
                        brokerAddress :: String,
                        brokerPort :: Int,
                        brokerSockAddr :: SockAddr } deriving (Show)

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
    sockaddr <- getSockAddr family broker_address broker_port_raw
    let our_port = if family == WAFamilyUnix
                    then 0
                    else parsePort our_port_raw
    let broker_port = if family == WAFamilyUnix
                       then 0
                       else parsePort broker_port_raw
    let config = WAConfig { bindFamily = family,
                            bindAddress = our_address,
                            bindPort = our_port,
                            brokerAddress = broker_address,
                            brokerPort = broker_port,
                            brokerSockAddr = sockaddr }
    if checkConfig config
     then return config
     else fail "Environment configuration error; aborting (hint: use the original env_vars script)"

parseFamily :: String -> Maybe WebappAddressFamily
parseFamily "UNIX" = Just WAFamilyUnix
parseFamily "INET" = Just WAFamilyInet
parseFamily _      = Nothing

parsePort :: String -> Int
parsePort raw | all isNumber raw = read raw
              | otherwise = 0

getSockAddr :: WebappAddressFamily -> String -> String -> IO SockAddr
getSockAddr WAFamilyUnix addr _ = return $ SockAddrUnix addr
getSockAddr WAFamilyInet addr port = do
    addrinfos <- getAddrInfo  (Just (defaultHints { addrFamily = AF_UNSPEC })) (Just addr) (Just port)
    if null addrinfos
     then fail "Couldn't obtain broker's address family"
     else return (addrAddress . head $ addrinfos)

checkConfig :: WebappConfiguration -> Bool
checkConfig conf =
       not (null (bindAddress conf))
    && not ( null (brokerAddress conf))
    && (bindFamily conf == WAFamilyUnix
            || ( -- If family is INET, then the ports must be filled
                   bindFamily conf == WAFamilyInet
                && bindPort conf /= 0
                && brokerPort conf /= 0
               )
       )

