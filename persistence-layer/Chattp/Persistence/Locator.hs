{-# LANGUAGE OverloadedStrings #-}

module Chattp.Persistence.Locator
( loginUser
, logoutUser
, lookupUser
) where

import Chattp.Persistence.Auth (checkUserExistence)

import Control.Applicative
import Data.Maybe
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Database.Redis


loginUser :: ByteString -> ByteString -> ByteString -> Redis [ByteString]
loginUser user broker channel = do
    ret <- hmset (userKey user) [("broker", broker), ("channel", channel)]
    return $ case ret of Right Ok -> ["OK"]
                         _        -> ["FAIL"]

logoutUser :: ByteString -> Redis [ByteString]
logoutUser user = del [userKey user] >> return ["OK"]


lookupUser :: ByteString -> Redis [ByteString]
lookupUser user = fromMaybe <$> lookupOffline user <*> lookupOnline user

lookupOnline :: ByteString -> Redis (Maybe [ByteString])
lookupOnline user = do
    ret <- exists $ userKey user
    case ret of Right True  -> Just <$> lookupOnline' user
                Right False -> return Nothing
                _           -> return $ Just ["FAIL"]

-- TODO: db error would cause crash; existence of hash fields is assumed
lookupOnline' :: ByteString -> Redis [ByteString]
lookupOnline' user = do
    Right [Just broker, Just channel] <- hmget (userKey user) ["broker", "channel"]
    return ["OK", broker, channel]

lookupOffline :: ByteString -> Redis [ByteString]
lookupOffline = fmap lookupOffline' . checkUserExistence

lookupOffline' :: Bool -> [ByteString]
lookupOffline' True = ["OFFLINE"]
lookupOffline' False = ["FAIL"]


userKey :: ByteString -> ByteString
userKey = B.append "user."

