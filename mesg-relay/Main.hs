module Main where

import Chattp.Relay.Config

main = do
    conf <- makeConfig
    print conf
