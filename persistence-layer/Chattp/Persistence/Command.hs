{-# LANGUAGE OverloadedStrings #-}

module Chattp.Persistence.Command
( runCmd
) where

import Chattp.Persistence.Auth
import Chattp.Persistence.Locator

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)

import Database.Redis


data CmdToken = UREG
              | CHKPASS
              | LOGIN
              | LOGOUT
              | ULKUP

parseCmdToken :: ByteString -> CmdToken
parseCmdToken "UREG"    = UREG
parseCmdToken "CHKPASS" = CHKPASS
parseCmdToken "LOGIN"   = LOGIN
parseCmdToken "LOGOUT"  = LOGOUT
parseCmdToken "ULKUP"   = ULKUP

getAnswerToken :: CmdToken -> ByteString
getAnswerToken UREG    = "UREGD"
getAnswerToken CHKPASS = "CHKDPASS"
getAnswerToken LOGIN   = "LGDIN"
getAnswerToken LOGOUT  = "LGDOUT"
getAnswerToken ULKUP   = "ULKDUP"

runCmd' :: CmdToken -> [ByteString] -> Redis [ByteString]
runCmd' UREG    [user, password]        = regUser user password
runCmd' CHKPASS [user, password]        = authUser user password
runCmd' LOGIN   [user, broker, channel] = loginUser user broker channel
runCmd' LOGOUT  [user]                  = logoutUser user
runCmd' ULKUP   [user]                  = lookupUser user

runCmd :: Connection -> [ByteString] -> IO [ByteString]
runCmd conn (cmd:args) = runRedis conn $ (answer :) <$> runCmd' cmdt args
    where cmdt = parseCmdToken cmd
          answer = getAnswerToken cmdt
