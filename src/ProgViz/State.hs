module ProgViz.State (Result, State, get, set, update, env) where

import qualified Control.Monad.State as S

import qualified Data.Map            as M

import           Text.Printf

import           ProgViz.Types

type Name = String

-- | The values of all the variables.
type State = M.Map Name Value

type Result a = S.State State a

get :: Name -> Result Value
get name = S.get >>= \ σ -> case M.lookup name σ of
  Just res -> return res
  Nothing  -> error (printf "Variable %s does not exist!" name)

set :: Name -> Value -> Result ()
set name value = S.modify $ M.insert name value

update :: Name -> (Value -> Value) -> Result ()
update name fn = S.modify $ M.update (return . fn) name

-- | Return the current environment.
env :: Result State
env = S.get
