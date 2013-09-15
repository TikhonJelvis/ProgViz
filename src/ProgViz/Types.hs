module ProgViz.Types where

import           Data.Functor ((<$>))
import           Data.List    (intercalate)

import           Text.Printf

-- | Statements in our simplified, desugared subset of Python. 
data Statement = Assign Expr Expr
               | Expr Expr
               | Seq Statement Statement
               | For String Expr Statement deriving (Show, Eq)

combine :: [Statement] -> Statement
combine = foldl1 Seq

-- | Expressions in our simplified, desugared subset of Python.
data Expr = Var String
          | NumLit Integer
          | StrLit String
          | BoolLit Bool
          | ListLit [Expr]
          | Index String Expr
          | Call String Method [Expr]
          | Bin Op Expr Expr deriving (Show, Eq)

-- | We only support a few built-in methods.
data Method = Add
            | Remove
            | Push
            | Pop
            | Shift
            | Unshift
            | Empty
            | Start
            | Mark
            | Neighbors deriving (Show, Eq)

toMethod :: String -> Method
toMethod "add"       = Add
toMethod "remove"    = Remove
toMethod "push"      = Push
toMethod "pop"       = Pop
toMethod "shift"     = Shift
toMethod "unshift"   = Unshift
toMethod "empty"     = Empty
toMethod "start"     = Start
toMethod "mark"      = Mark
toMethod "neighbors" = Neighbors
toMethod _           = error "Unknown method name!"

data Op = Plus | Minus | Mult | Gt | Lt | Geq | Leq | Eq | Neq deriving (Show, Eq)

-- | Runtime values in our tiny Python subset.
data Value = Num Integer
           | Str String
           | Bool Bool
           | List [Value]
           | Node Int String Bool Value
           | Graph [Value] [(Int, Int)] deriving (Eq, Ord)

setNode :: Value -> Int -> Value -> Value
setNode (Graph nodes edges) idn new = Graph (updateNode <$> nodes) edges
  where updateNode node@(Node idn' _ _ _) = if idn' == idn then new else node
        updateNode _ = error "Graphs should only contain nodes!"
setNode _ _ _ = error "You can only set the node in a graph!"

instance Show Value where
  show (Num n)             = show n
  show (Str s)             = s
  show (Bool b)            = show b
  show (List ls)           = intercalate "," $ show <$> ls
  show (Node n graph b v)  = printf "<%d,%s, %s, %s>" n graph (show b) (show v)
  show (Graph nodes edges) = show nodes ++ "\n" ++ show edges

listGet :: Value -> Value -> Value
listGet (List values) (Num index) = values !! fromInteger index
listGet _ _ = error "Wrong types!"

liftNumOp :: (Integer -> Integer -> Integer) -> (Value -> Value -> Value)
liftNumOp (⊗) (Num n₁) (Num n₂) = Num $ n₁ ⊗ n₂
liftNumOp _ _ _                 = error "Wrong types!"

liftBoolOp :: (Bool -> Bool -> Bool) -> (Value -> Value -> Value)
liftBoolOp (⊗) (Bool b₁) (Bool b₂) = Bool $ b₁ ⊗ b₂
liftBoolOp _ _ _                   = error "Wrong types!" 

liftNumFn :: (Integer -> Integer) -> (Value -> Value)
liftNumFn fn (Num n) = Num $ fn n
liftNumFn _ _        = error "Wrong types!"

instance Num Value where
  (+)         = liftNumOp (+) 
  (-)         = liftNumOp (-) 
  (*)         = liftNumOp (*)
  abs         = liftNumFn (abs)
  signum      = liftNumFn (signum)
  fromInteger = Num
