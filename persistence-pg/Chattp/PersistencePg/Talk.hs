{-# LANGUAGE OverloadedStrings #-}

module Chattp.PersistencePg.Talk where

import qualified Data.Sequence as Seq
import Data.Foldable (toList)

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as SBS

import Control.Exception.Base

import Control.Monad (liftM)
import Data.Functor ((<$))

import Chattp.PersistencePg.Database

import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.WireMessage

import Chattp.PersistenceRequest as Rq
import Chattp.PersistenceRequest.PersistenceRequestType
import Chattp.PersistenceResponse as Rp
import Chattp.PersistenceResponse.PersistenceResponseType

import Database.PostgreSQL.Simple

{-
data PersistenceResponse = PersistenceResponse{sequence_number :: !P'.Word64,
                                               type' :: !Chattp.PersistenceResponse.PersistenceResponseType,
                                               status :: !(P'.Maybe P'.Bool),
                                               user_locations :: !(P'.Seq Chattp.PersistenceResponse.UserLocation),
                                               mesgs :: !(P'.Seq Chattp.ChattpMessage), settings :: !(P'.Maybe P'.Utf8)}

data PersistenceRequest = PersistenceRequest{sequence_number :: !P'.Word64,
                                            type' :: !Chattp.PersistenceRequest.PersistenceRequestType,
                                            user_name :: !(P'.Maybe P'.Utf8), password :: !(P'.Maybe P'.Utf8),
                                            email :: !(P'.Maybe P'.Utf8), channel_id :: !(P'.Maybe P'.Utf8),
                                            broker_name :: !(P'.Maybe P'.Utf8), lookup_users :: !(P'.Seq P'.Utf8),
                                            mesg :: !(P'.Maybe Chattp.ChattpMessage), settings :: !(P'.Maybe P'.Utf8)}

-}

handleMessage :: Connection -> SBS.ByteString -> IO (Maybe SBS.ByteString)
handleMessage conn msg = case messageGet (BS.fromStrict msg) of
                        Left e -> Nothing <$ putStrLn ("Message parse error: " ++ e)
                        Right (m,rest) | rest == BS.empty -> do
                                                resp <- handleRequest conn (Rq.type' m) m
                                                --print resp
                                                return . Just . BS.toStrict . messagePut $ resp
                                       | otherwise -> Nothing <$ putStrLn "Message parse error: Couldn't parse complete message."

handleRequest :: Connection -> PersistenceRequestType -> PersistenceRequest -> IO PersistenceResponse
-- REGISTER
handleRequest conn REGISTER msg = do
    --print msg
    withTransaction conn $ do
        exists <- userExists (maybe "" uToString $ Rq.user_name msg) conn
        if exists || null (maybe "" uToString $ Rq.user_name msg) || null (maybe "" uToString $ Rq.password msg)
            then return $ (defaultAnswer msg) { Rp.status = Just False, Rp.type' = REGISTERED }
            else do
                stat <- catch (registerUser ( maybe "" uToString $ Rq.user_name msg,
                                                maybe "" uToString $ Rq.password msg,
                                                maybe "" uToString $ Rq.email msg ) conn ) handleSQLError
                return $ (defaultAnswer msg) { Rp.status = Just stat, Rp.type' = REGISTERED }
-- LOGIN
handleRequest conn LOGIN msg = do
    --print msg
    withTransaction conn $ do
        exists <- userExists (maybe "" uToString $ Rq.user_name msg) conn
        if not exists || null (maybe "" uToString $ Rq.user_name msg) || null (maybe "" uToString $ Rq.broker_name msg) || null (maybe "" uToString $ Rq.channel_id msg)
            then return $ (defaultAnswer msg) { Rp.status = Just False, Rp.type' = LOGGEDIN }
            else do
                if not $ null (maybe "" uToString $ Rq.password msg) -- password was sent; check password.
                    then do
                        pwd_status <- catch (checkPassword (maybe "" uToString $ Rq.user_name msg,
                                                            maybe "" uToString $ Rq.password msg) conn ) handleSQLError
                        if not pwd_status
                            then return $ (defaultAnswer msg) { Rp.status = Just False, Rp.type' = REGISTERED }
                            else doLogin
                    else doLogin -- no password was sent
    where doLogin = do
            login_status <- catch (loginUser (maybe "" uToString $ Rq.user_name msg,
                                              maybe "" uToString $ Rq.broker_name msg,
                                              maybe "" uToString $ Rq.channel_id msg) conn) handleSQLError
            return $ (defaultAnswer msg) { Rp.status = Just login_status, Rp.type' = LOGGEDIN }
-- LOGOUT
handleRequest conn LOGOUT msg = do
    --print msg
    stat <- catch (logoutUser (maybe "" uToString $ Rq.user_name msg,
                                 maybe "" uToString $ Rq.channel_id msg) conn) handleSQLError
    return $ (defaultAnswer msg) { Rp.status = Just stat, Rp.type' = LOGGEDOUT }
-- CHECKPASS
handleRequest conn CHECKPASS msg = do
    --print msg
    stat <- catch (checkPassword (maybe "" uToString $ Rq.user_name msg,
                                    maybe "" uToString $ Rq.password msg) conn) handleSQLError
    return $ (defaultAnswer msg) { Rp.status = Just stat, Rp.type' = CHECKEDPASS }
-- LOOKUP
handleRequest conn LOOKUP msg = do
    --print msg
    withTransaction conn $ do
        exist <- mapM (\u -> userExists (uToString u) conn) (toList (Rq.lookup_users msg)) -- very inefficient, better solution?
        if and exist
            then do
                locs <- liftM concat $ mapM lookupOne (toList (Rq.lookup_users msg))
                return $ (defaultAnswer msg) { Rp.status = Just True, Rp.user_locations = Seq.fromList locs, Rp.type' = LOOKEDUP }
            else return $ (defaultAnswer msg) { Rp.status = Just False, Rp.type' = LOOKEDUP }
    where lookupOne usr = catch (lookupUser (uToString usr) conn) (\e -> [] <$ handleSQLError e)
-- SAVEMESSAGE
handleRequest conn SAVEMESSAGE msg = do
    --print msg
    stat <- catch (saveMessage (fromMaybe defaultValue $ Rq.mesg msg) conn) handleSQLError
    return $ (defaultAnswer msg) { Rp.status = Just stat, Rp.type' = SAVEDMESSAGE }
-- GETMESSAGES
handleRequest conn GETMESSAGES msg = do
    --print msg
    withTransaction conn $ do
        msgs <- catch (getMessages (maybe "" uToString $ Rq.user_name msg) conn) (([]<$) . handleSQLError)
        _ <- catch (deleteMessages (maybe "" uToString $ Rq.user_name msg) conn) handleSQLError
        return $ (defaultAnswer msg) { Rp.status = Just True, Rp.type' = GOTMESSAGES, Rp.mesgs = Seq.fromList msgs }
-- SAVESETTINGS
handleRequest conn SAVESETTINGS msg = do
    --print msg
    stat <- catch (saveSettings (maybe "" uToString $ Rq.user_name msg, maybe "" utf8 $ Rq.settings msg) conn) handleSQLError
    return $ (defaultAnswer msg) { Rp.status = Just stat, Rp.type' = SAVEDSETTINGS }
-- GETSETTINGS
handleRequest conn GETSETTINGS msg = do
    --print msg
    sets <- catch (getSettings (maybe "" uToString $ Rq.user_name msg) conn) ((Nothing<$) . handleSQLError)
    case sets of
        Nothing -> return $ (defaultAnswer msg) { Rp.status = Just False, Rp.type' = GOTSETTINGS }
        Just set -> return $ (defaultAnswer msg) { Rp.status = Just True, Rp.type' = GOTSETTINGS, Rp.settings = Just $ unsafeToUtf8 set }

defaultAnswer :: PersistenceRequest -> PersistenceResponse
defaultAnswer msg = defaultValue { Rp.sequence_number = Rq.sequence_number msg }

handleSQLError :: SqlError -> IO Bool
handleSQLError e = SBS.putStrLn (sqlErrorMsg e `SBS.append` sqlErrorDetail e) >> return False

