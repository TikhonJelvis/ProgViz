module ProgViz.Interpreter where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (forM)
import           Control.Monad.State (evalState)

import           Data.List
import qualified Data.Map            as M

import           ProgViz.State       (Result)
import qualified ProgViz.State       as S
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
step (For var lsExpr body) = do
  List ls <- eval lsExpr
  fmap List . forM ls $ \ val -> S.set var val >> step body

-- | Given code before a loop and the loop itself, return all the
--   loop's intermediate states.
runLoop :: Statement -> Statement -> (String, [S.State])
runLoop prelude (For var lsExpr body) =
  let states = do
        _ <- step prelude
        List ls <- eval lsExpr
        forM ls $ \ val -> S.set var val >> step body >> S.env
  in
  (var, evalState states M.empty)
runLoop _ _                           = error "Please supply a loop!"

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
doMethod Add name args = do
  List vals <- S.get name
  let new = List $ vals ++ args
  S.set name new
  return new
doMethod Pop name [] = do
  List (v:vs) <- S.get name
  S.set name $ List vs
  return v
doMethod Push name [v] = do
  List vs <- S.get name
  let new = List $ v:vs
  S.set name new
  return new
doMethod Shift name [] = do
  List vs <- S.get name
  S.set name . List $ init vs
  return $ last vs
doMethod Unshift name [v] = do
  List vs <- S.get name
  let new = List $ vs ++ [v]
  S.set name new
  return new
doMethod Empty name [] = do
  List vals <- S.get name
  return . Bool $ null vals
doMethod Start name [] = do
  Graph _ (n:_) _ <- S.get name
  return n
doMethod Mark name [] = do
  Node idn graph _ <- S.get name
  let new = Node idn graph True
  g <- S.get graph
  let g' = setNode g idn new
  S.set graph g'
  return new
doMethod Neighbors name [] = do
  Node idn graph _2 <- S.get name
  Graph _ nodes edges <- S.get graph
  let idns = snd <$> filter (\ (a, _) -> a == idn) edges
  return . List $ filter (\ (Node idn' _ _) -> idn' `elem` idns) nodes
doMethod Create "Graph" [Str name, List edges] =
  return $ Graph name (node . toInt <$> nub edges) (pairs $ toInt <$> edges)
  where node idn = Node idn name False
        toInt (Num n) = n
        toInt _       = error "Wrong type!"
doMethod _ _ _               = error "Invalid method call!" 

pairs :: [a] -> [(a,a)]
pairs []       = []
pairs [_]      = []
pairs (a:b:xs) = (a, b) : pairs xs
