module Main where

import Control.Monad

import Chattp.Relay.Config
import Chattp.Relay.Router

import Control.Concurrent

main :: IO ()
main = do
    conf <- makeConfig
    print conf
    rconf <- makeRouterConfig conf
    replicateM_ (if (nThreads conf - 1) < 0 then 0 else (nThreads conf - 1)) (forkOS (router Nothing rconf))
    router Nothing rconf

