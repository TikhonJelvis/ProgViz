module ProgViz.Types where

-- | Statements in our simplified, desugared subset of Python. 
data Statement = Assign Expr Expr
               | Seq Statement Statement deriving (Show, Eq)

-- | Expressions in our simplified, desugared subset of Python.
data Expr = Var String
          | Index String Expr
          | Call Expr Method [Expr]
          | Bin Op Expr Expr deriving (Show, Eq)

-- | We only support a few built-in methods.
data Method = Add
            | Remove deriving (Show, Eq)

data Op = Plus | Minus | Mult | Ge | Le | Geq | Leq | Eq | Neq deriving (Show, Eq)

-- | Runtime values in our tiny Python subset.
data Value = Num Integer
           | Str String
           | Bool Bool
           | List [Value] deriving (Show, Eq, Ord)

listGet :: Value -> Value -> Value
listGet (List values) (Num index) = values !! fromInteger index
listGet _ _ = error "Wrong types!"

liftNumOp :: (Integer -> Integer -> Integer) -> (Value -> Value -> Value)
liftNumOp (⊗) (Num n₁) (Num n₂) = Num $ n₁ ⊗ n₂
liftNumOp _ _ _                 = error "Wrong types!"

liftNumFn fn (Num n) = Num $ fn n
liftNumFn _ _        = error "Wrong types!"

instance Num Value where
  (+)         = liftNumOp (+) 
  (-)         = liftNumOp (-) 
  (*)         = liftNumOp (*)
  abs         = liftNumFn (abs)
  signum      = liftNumFn (signum)
  fromInteger = Num
  
