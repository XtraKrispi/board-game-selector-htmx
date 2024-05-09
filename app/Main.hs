module Main where

import App (app)
import Control.Concurrent.STM (newTVarIO)
import Data.IntMap qualified as Map
import Web.Scotty

main :: IO ()
main = do
  cache <- newTVarIO Map.empty
  scotty 3000 (app cache)
