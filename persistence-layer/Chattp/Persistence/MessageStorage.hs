{-# LANGUAGE OverloadedStrings #-}

module Chattp.Persistence.MessageStorage where

import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class
import Control.Monad

import Database.Redis hiding (decode)

import Data.Vector (fromList)
import Data.ByteString.Char8 as BS hiding (map)
import Data.ByteString.Lazy.Char8 (toStrict,fromStrict)

import Data.Text.Encoding (decodeUtf8)
import Data.UnixTime (getUnixTime,formatUnixTimeGMT,webDateFormat)
import Data.Aeson hiding (Error)


--              Receiver        Sender      Message
saveMessage :: ByteString -> ByteString -> ByteString -> Redis [ByteString]
saveMessage destusr sndusr msg = do
    json_msg <- liftIO $ messageToJSON sndusr msg
    result <- rpush (getUserMesgKey destusr) [json_msg]
    case result of
        Right _ -> return ["OK"]
        Left _ -> return ["FAIL"]

getMessages :: ByteString -> Redis [ByteString]
getMessages usr = do
    mesgs <- getAll (getUserMesgKey usr) []
    let json_obj = toStrict . encode . Array . fromList $ map (fromMaybe Null . decode . fromStrict) mesgs
    return ["OK",json_obj]
    where getAll :: ByteString -> [ByteString] -> Redis [ByteString]
          getAll key acc = rpop key >>= \val -> case val of
                                                    Right (Just val') -> getAll key (val':acc)
                                                    _ -> return acc

messageToJSON :: ByteString -> ByteString -> IO ByteString
messageToJSON sndusr msg = do
    tstamp <- getWebGMTTime
    let json_obj = object ["from" .= decodeUtf8 sndusr,
                           "timestamp" .= decodeUtf8 tstamp,
                           "message" .= decodeUtf8 msg]
    return . toStrict . encode $ json_obj

getWebGMTTime :: IO ByteString
getWebGMTTime = liftM (formatUnixTimeGMT webDateFormat) getUnixTime

getUserMesgKey :: ByteString -> ByteString
getUserMesgKey = BS.append "user_messages."

