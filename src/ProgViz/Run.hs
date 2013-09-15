module ProgViz.Run where

import qualified Control.Monad.State as S

import qualified Data.Map            as M
import           Data.Maybe          (mapMaybe)

import           ProgViz.Adapt
import           ProgViz.State
import           ProgViz.Types
import           ProgViz.Interpreter

run :: String -> Value
run = (`S.evalState` M.empty) . step . testParse

test :: String -> String -> (String, [State])
test prelude loop = runLoop (testParse prelude) (testParse loop)

-- Get the values of a given variable at all the intermediate steps.
test' :: String -> String -> String -> [Value]
test' name prelude loop =
  let (_, states) = test prelude loop in
  mapMaybe (M.lookup name) states
