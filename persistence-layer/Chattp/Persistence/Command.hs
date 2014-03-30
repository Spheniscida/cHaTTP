{-# LANGUAGE OverloadedStrings #-}

module Chattp.Persistence.Command
( runCmd
) where

import Chattp.Persistence.Auth

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)

import Database.Redis


data CmdToken = UREG
              | CHKPASS

parseCmdToken :: ByteString -> CmdToken
parseCmdToken "UREG"    = UREG
parseCmdToken "CHKPASS" = CHKPASS

getAnswerToken :: CmdToken -> ByteString
getAnswerToken UREG    = "UREGD"
getAnswerToken CHKPASS = "CHKDPASS"

runCmd' :: CmdToken -> [ByteString] -> Redis [ByteString]
runCmd' UREG    [user, password] = regUser  user password
runCmd' CHKPASS [user, password] = authUser user password

runCmd :: Connection -> [ByteString] -> IO [ByteString]
runCmd conn (cmd:args) = runRedis conn $ (answer :) <$> runCmd' cmdt args
    where cmdt = parseCmdToken cmd
          answer = getAnswerToken cmdt
