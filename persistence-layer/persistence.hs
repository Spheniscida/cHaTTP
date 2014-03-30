module Main where

import Chattp.Persistence.Command
import Chattp.Persistence.Config
import Chattp.Persistence.Interface

import Database.Redis


main :: IO ()
main = do
    sock <- uncurry initSocket =<< getInterfaceSockAddr
    conn <- connect =<< getRedisConnInfo
    loop sock conn

loop :: Socket -> Connection -> IO ()
loop sock conn = talk (runCmd conn) sock >> loop sock conn
