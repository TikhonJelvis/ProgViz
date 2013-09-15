module ProgViz.Interpreter where

import           Control.Applicative

import           ProgViz.State (Result)
import qualified ProgViz.State as S
import           ProgViz.Types

updateList :: Int -> a -> [a] -> [a]
updateList _ _ []         = error "Wrong index!"
updateList 0 value (_:xs) = value : xs
updateList n value (x:xs) = x : updateList (n - 1) value xs

-- | Execute an expression on the given state.
step :: Statement -> Result Value
step (Assign target expr) =
  do value <- eval expr
     case target of
       Var name     -> S.set name value
       Index name i -> do i' <- evalToInt i
                          S.update name $ set i' value
         where evalToInt intExpr =
                 do res <- eval intExpr
                    case res of
                      Num n -> return $ fromInteger n
                      _     -> error "Not a number!"
               set index new (List items)
                 | length items > index = List $ updateList index new items
               set _ _ _ = error "Invalid index!"
       _ -> error "Invalid syntax!"
     return value
step (Expr e) = eval e
step (Seq s₁ s₂) = step s₁ >> step s₂

eval :: Expr -> Result Value
eval (Var name)           = S.get name
eval (NumLit n)           = return $ Num n
eval (StrLit s)           = return $ Str s
eval (BoolLit b)          = return $ Bool b
eval (ListLit ls)         = List <$> mapM eval ls
eval (Index name i)       = listGet <$> S.get name <*> eval i
eval (Bin op e₁ e₂)       = operate op <$> eval e₁ <*> eval e₂
eval (Call e method args) = mapM eval args >>= doMethod method e

-- | Evaluate a binary operator.
operate :: Op -> Value -> Value -> Value
operate Plus  =      (+)
operate Minus =      (-)
operate Mult  =      (*)
operate Gt    = wrap (>)
operate Lt    = wrap (<)
operate Geq   = wrap (>=)
operate Leq   = wrap (<=)
operate Eq    = wrap (==)
operate Neq   = wrap (/=)

wrap :: (Value -> Value -> Bool) -> (Value -> Value -> Value)
wrap (⊗) v₁ v₂ = Bool $ v₁ ⊗ v₂

doMethod :: Method -> String -> [Value] -> Result Value
doMethod Add name args = do List vals <- S.get name
                            let new = List $ vals ++ args
                            S.set name new
                            return new
doMethod _ _ _               = error "Invalid method call!" 
