{-# LANGUAGE OverloadedStrings #-}

module Chattp.PersistencePg.Database where

import Data.ByteString.Lazy.Char8 as BS

import Control.Monad (liftM)

import Database.PostgreSQL.Simple

import Text.ProtocolBuffers.Header hiding (ByteString,emptyBS,pack)
import Chattp.PersistenceResponse.UserLocation as UL
import Chattp.ChattpMessage as Msg

{-
data UserLocation = UserLocation{online :: !(P'.Maybe P'.Bool), user_name :: !(P'.Maybe P'.Utf8),
                                 broker_name :: !(P'.Maybe P'.Utf8), channel_id :: !(P'.Maybe P'.Utf8)}

data ChattpMessage = ChattpMessage{sender :: !P'.Utf8, receiver :: !P'.Utf8, timestamp :: !P'.Utf8, body :: !(P'.Maybe P'.Utf8),
                                   group_message :: !(P'.Maybe P'.Bool), is_typing :: !(P'.Maybe P'.Bool),
                                   has_seen :: !(P'.Maybe P'.Bool)}
-}

userExistsQuery :: Query -- Parameters: user_name
userExistsQuery = "SELECT count(*) FROM chattp_users WHERE user_name = ?"

userRegisterQuery :: Query -- Parameters: user_name, user_password (clear text), user_email
userRegisterQuery = "INSERT INTO chattp_users (user_name,user_password,user_email) VALUES (?,crypt(?,gen_salt('bf')),?)"

checkPasswordQuery :: Query -- Parameters: user_password, user_name
checkPasswordQuery = "SELECT user_password = crypt(?,user_password) FROM chattp_users WHERE user_name = ?"

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

userLookupQuery :: Query -- Parameters: user_name
userLookupQuery = "SELECT broker_name, channel_id FROM chattp_locations JOIN chattp_users USING (user_id) JOIN chattp_brokers USING (broker_id) WHERE user_name = ?"

saveMessageQuery :: Query -- Parameters: timestamp, group_message, body, receiver, sender
saveMessageQuery = "INSERT INTO chattp_messages (receiver,sender,timestamp,group_message,body) SELECT r.user_id, s.user_id, ?, ?, ? FROM chattp_users AS r, chattp_users AS s WHERE r.user_name = ? AND s.user_name = ?"

getMessagesQuery :: Query -- Parameter: user_name (receiver)
getMessagesQuery = "SELECT r.user_name, s.user_name, timestamp, group_message, body FROM chattp_messages JOIN chattp_users AS r ON (receiver = r.user_id) JOIN chattp_users AS s ON (sender = s.user_id) WHERE receiver = (SELECT user_id FROM chattp_users WHERE user_name = ?)"

deleteMessagesQuery :: Query -- Parameter: user_name
deleteMessagesQuery = "DELETE FROM chattp_messages WHERE receiver = (SELECT user_id FROM chattp_users WHERE user_name = ?)"

saveSettingsQuery :: Query -- Parameters: settings, user_name
saveSettingsQuery = "UPDATE chattp_users SET user_settings = ? WHERE user_name = ?"

getSettingsQuery :: Query -- Parameter: user_name
getSettingsQuery = "SELECT coalesce(user_settings,'') FROM chattp_users WHERE user_name = ? AND user_settings IS NOT NULL"

-------------------------------------------------------------------------------

userExists :: String -> Connection -> IO Bool
userExists usr conn = (query conn userExistsQuery (Only usr) :: IO [Only Int]) >>= \(Only n :_) -> return (n > 0)

registerUser :: (String,String,String) -> Connection -> IO Bool
registerUser credentials@(_usr,_pwd,_email) conn = liftM (>0) $ execute conn userRegisterQuery credentials

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

createBroker :: String -> Connection -> IO Bool
createBroker broker conn = liftM (>0) $ execute conn brokerCreateQuery (Only broker)

logoutUser :: (String,String) -> Connection -> IO Bool
logoutUser (usr,channelid) conn = case channelid of
                                    [] -> liftM (>0) $ execute conn userLogoutQuery (Only usr)
                                    _  -> liftM (>0) $ execute conn userLogoutChanIDQuery (usr,channelid)

checkPassword :: (String,String) -> Connection ->  IO Bool
checkPassword (usr,pwd) conn = query conn checkPasswordQuery (pwd,usr) >>= \(Only p : _) -> return p

lookupUser :: String -> Connection -> IO [UserLocation]
lookupUser usr conn = getLocList >>= \l -> case l of
                                            [] -> return [defaultValue { online = Just False, user_name = Just $ uFromString usr }]
                                            locs -> return locs
    where getLocList = fold conn userLookupQuery (Only usr) [] (\locs (broker,channel) -> return $ (defaultValue { online = Just True,
                                                                                                                   user_name = Just $ uFromString usr,
                                                                                                                   broker_name = Just $ uFromString broker, 
                                                                                                                   channel_id = Just $ uFromString channel }) : locs )
saveMessage :: ChattpMessage -> Connection -> IO Bool
saveMessage msg conn = liftM (>0) $  execute conn saveMessageQuery (uToString (timestamp msg),
                                                      fromMaybe False (group_message msg),
                                                      maybe "" utf8 (body msg),
                                                      uToString (receiver msg),
                                                      uToString (sender msg))

getMessages :: String -> Connection -> IO [ChattpMessage]
getMessages usr conn = fold conn getMessagesQuery (Only usr) []
    (\msgs (rcvr,sndr,tmstmp,grp_msg,bdy) -> 
        return $ (defaultValue { receiver = unsafeToUtf8 rcvr,
                                 sender = unsafeToUtf8 sndr,
                                 timestamp = unsafeToUtf8 tmstmp,
                                 group_message = Just grp_msg,
                                 body = if bdy == BS.empty then Nothing else Just $ unsafeToUtf8 bdy }) : msgs)

deleteMessages :: String -> Connection -> IO Bool
deleteMessages usr conn = execute conn deleteMessagesQuery (Only usr) >> return True

saveSettings :: (String,ByteString) -> Connection -> IO Bool
saveSettings (usr,settngs) conn = liftM (>0) $ execute conn saveSettingsQuery (BS.toStrict settngs,usr)

getSettings :: String -> Connection -> IO (Maybe BS.ByteString)
getSettings usr conn = query conn getSettingsQuery (Only usr) >>= \l -> case l of
                                                                            [] -> return Nothing
                                                                            (Only s : _) -> return (Just s)

-- Helpers


unsafeToUtf8 :: BS.ByteString -> Utf8
unsafeToUtf8 b = case toUtf8 b of
                    Right u -> u
                    Left _i -> uFromString ""

