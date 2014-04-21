module Main where

import Chattp.Webapp.Conf
import Chattp.Webapp.IPC
import Chattp.Webapp.Protocol

main = do
    config <- getConfig
    print config
    sock <- createWebappSocket config
    return ()
