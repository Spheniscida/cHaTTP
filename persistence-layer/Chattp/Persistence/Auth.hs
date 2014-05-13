{-# LANGUAGE OverloadedStrings #-}

module Chattp.Persistence.Auth
( authUser
, regUser
, checkUserExistence
) where

import Control.Applicative
import Control.Monad.Trans
import Data.ByteString.Char8 (ByteString)

import Crypto.PasswordStore
import Database.Redis


type Hash = ByteString
type Password = ByteString
type User = ByteString


authUser :: User -> Password -> Redis [ByteString]
authUser u p = maybe ["FAIL"] (checkPassword p) <$> getHash u

getHash :: User -> Redis (Maybe Hash)
getHash = fmap maybeRight . hget "pwds"

checkPassword :: Password -> Hash -> [ByteString]
checkPassword p h | checkPassword' p h = ["OK"]
                  | otherwise          = ["FAIL"]

checkPassword' :: Password -> Hash -> Bool
checkPassword' = verifyPasswordWith pbkdf2 (2^)


regUser :: User -> Password -> Redis [ByteString]
regUser u p = storeHash u =<< liftIO (hashPassword p)

storeHash :: User -> Hash -> Redis [ByteString]
storeHash u h = eitherConst ["FAIL"] testSuccess <$> hsetnx "pwds" u h
    where testSuccess True  = ["OK"]
          testSuccess False = ["FAIL"]

hashPassword :: Password -> IO Hash
hashPassword p = makePasswordWith pbkdf2 p strength
    where strength = 14


-- TODO: shouldn't return False on db error
checkUserExistence :: User -> Redis Bool
checkUserExistence = fmap (== Right True) . hexists "pwds"


eitherConst :: c -> (b -> c) -> Either a b -> c
eitherConst c = either (const c)

maybeRight :: Either a (Maybe b) -> Maybe b
maybeRight = eitherConst Nothing id

