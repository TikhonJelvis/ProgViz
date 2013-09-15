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
step :: Statement -> Result ()
step (Assign target expr) =
  case target of
    Var name     -> S.set name =<< value
    Index name i -> do i' <- evalToInt i
                       S.update name . set i' =<< value
      where evalToInt expr =
              do res <- eval expr
                 case res of
                   Num i -> return $ fromInteger i
                   _     -> error "Not a number!"
            set index value (List items)
              | length items > index = List $ updateList index value items
            set _ _ _ = error "Invalid index!"
  where value = eval expr

eval :: Expr -> Result Value
eval (Var name)           = S.get name
eval (Index name i)       = listGet <$> S.get name <*> eval i
eval (Bin op e₁ e₂)       = operate op <$> eval e₁ <*> eval e₂
eval (Call e method args) = mapM eval args >>= doMethod method e

-- | Evaluate a binary operator.
operate :: Op -> Value -> Value -> Value
operate Plus  =      (+)
operate Minus =      (-)
operate Mult  =      (*)
operate Ge    = wrap (>)
operate Le    = wrap (<)
operate Geq   = wrap (>=)
operate Leq   = wrap (<=)
operate Eq    = wrap (==)
operate Neq   = wrap (/=)

wrap :: (Value -> Value -> Bool) -> (Value -> Value -> Value)
wrap (⊗) v₁ v₂ = Bool $ v₁ ⊗ v₂

doMethod :: Method -> Expr -> [Value] -> Result Value
doMethod Add (Var name) args = do List vals <- S.get name
                                  let new = List $ vals ++ args
                                  S.set name new
                                  return new
doMethod _ _ _               = error "Invalid method call!" 
