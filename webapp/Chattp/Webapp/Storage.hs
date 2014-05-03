{-# LANGUAGE OverloadedStrings #-}

{-
    - This module interacts with a Redis database for storing
    arbitrary data.
    Right now, its only purpose is storing and fetching user settings.
-}

module Chattp.Webapp.Storage
(storeUserSettings,
 getUserSettings
) where

import Chattp.Webapp.Protocol

import Database.Redis

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BSS

storeUserSettings :: Connection -> UserName -> BS.ByteString -> IO String
storeUserSettings conn u s = runRedis conn $ storeUserSettings' u s
    where storeUserSettings' :: UserName -> BS.ByteString -> Redis String
          storeUserSettings' usr settings = do
                                    result <- set (userToKey usr) (BS.toStrict settings)
                                    case result of
                                        Right _i -> return ""
                                        Left (Error e) -> return (BSS.unpack e)
                                        Left e -> fail $ "Unknown error in redis: set. " ++ show e

getUserSettings :: Connection -> UserName -> IO BS.ByteString
getUserSettings conn = runRedis conn . getUserSettings'
    where getUserSettings' :: UserName -> Redis BS.ByteString
          getUserSettings' usr = do
                                result <- get $ userToKey usr
                                case result of
                                    Right (Just set_s) -> return . BS.fromStrict $ set_s
                                    Right Nothing -> return ""
                                    Left _ -> return ""

userToKey :: UserName -> BSS.ByteString
userToKey = BSS.append "user_settings." . BS.toStrict

