module Main where

import Chattp.Webapp.Conf
import Chattp.Webapp.IPC

main = getConfig >>= print
