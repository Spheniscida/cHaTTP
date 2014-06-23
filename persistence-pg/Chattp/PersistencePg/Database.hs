{-# LANGUAGE OverloadedStrings #-}

module Chattp.PersistencePg.Database where

import Control.Monad (void)

import Database.PostgreSQL.Simple

userExistsQuery :: Query -- Parameters: user_name
userExistsQuery = "SELECT count(*) FROM chattp_users WHERE user_name = ?"

userRegisterQuery :: Query -- Parameters: user_name, user_password (clear text), user_email
userRegisterQuery = "INSERT INTO chattp_users (user_name,user_password,user_email) VALUES (?,?,crypt(?,gen_salt('bf')))"

userLoginQuery :: Query -- Parameters: channel_id, user_name, broker_name
userLoginQuery = "INSERT INTO chattp_locations (user_id,broker_id,channel_id) (SELECT u.user_id, b.broker_id, ? FROM chattp_users AS u, chattp_brokers AS b WHERE u.user_name = ? AND b.broker_name = ?)"

brokerExistsQuery :: Query -- Parameters: broker_name
brokerExistsQuery = "SELECT count(*) FROM chattp_brokers WHERE broker_name = ?"

brokerCreateQuery :: Query -- Parameters: broker_name
brokerCreateQuery = "INSERT INTO chattp_brokers (broker_name) VALUES (?)"

userLogoutChanIDQuery :: Query -- Parameters: user_name, channel_id
userLogoutChanIDQuery = "DELETE FROM chattp_locations WHERE user_id = (SELECT user_id FROM chattp_users WHERE user_name = ?) AND channel_id = ?"

userLogoutQuery :: Query -- Parameters: user_name
userLogoutQuery = "DELETE FROM chattp_locations WHERE user_id = (SELECT user_id FROM chattp_users WHERE user_name = ?)"

-------------------------------------------------------------------------------


userExists :: String -> Connection -> IO Bool
userExists usr conn = (query conn userExistsQuery (Only usr) :: IO [Only Int]) >>= \(Only n :_) -> return (n > 0)

createUser :: (String,String,String) -> Connection -> IO ()
createUser credentials@(_usr,_pwd,_email) conn = void $ execute conn userRegisterQuery credentials

-- Returns True if user is logged in, False if user doesn't exist
loginUser :: (String,String,String) -> Connection -> IO Bool
loginUser (usr,broker,channel) conn = do
    broker_exists <- (query conn brokerExistsQuery (Only broker) :: IO [Only Int]) >>= \(Only n : _) -> return (n > 0)
    user_exists <- (query conn userExistsQuery (Only usr) :: IO [Only Int]) >>= \(Only n : _) -> return (n > 0)
    case (broker_exists,user_exists) of
        (True,True) -> doLogin
        (False,True) -> createBroker broker conn >> doLogin
        _ -> return False

    where doLogin = execute conn userLoginQuery (channel,usr,broker) >> return True

createBroker :: String -> Connection -> IO ()
createBroker broker conn = void $ execute conn brokerCreateQuery (Only broker)

logoutUser :: (String,String) -> Connection -> IO ()
logoutUser (usr,channelid) conn = case channelid of
                                    [] -> void $ execute conn userLogoutQuery (Only usr)
                                    _  -> void $ execute conn userLogoutChanIDQuery [usr,channelid]

