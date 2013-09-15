module ProgViz.Run where

import           Control.Monad.State

import qualified Data.Map            as M

import           ProgViz.Adapt
import           ProgViz.Types
import           ProgViz.Interpreter

run :: String -> Value
run = (`evalState` M.empty) . step . testParse . (++ "\n")
