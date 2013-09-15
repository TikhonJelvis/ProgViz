module ProgViz.UI where

import           Control.Monad

import           Data.IORef
import qualified Data.Map                      as M

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Events as E

import           System.Environment

import           Text.Printf

import           ProgViz.Run
import           ProgViz.State               (State)
import           ProgViz.Types

main :: IO ()
main = do
  [ n ] <- getArgs
  let pf = "p" ++ n
      bf = "b" ++ n
      port = 10000 + read n
  p <- readFile pf
  b <- readFile bf
  startGUI defaultConfig {
   tpStatic = Just "src/",
   tpCustomHTML = Just "html/index.html",
   tpPort = port
  } (setup p b)

prelude, body :: String
prelude = "g = Graph.Create('g', [1,2,1,3,1,4,1,5,5,3,3,6,6,7,7,8,8,5])\nx = 0\nls=[1,2,3,4,5,6,7]\nls2 = []"
body = "for i in ls:\n  i\n  x = 10 + x + i\n  ls2.add(i * 10)\n  n = g.start()\n  n.mark()"

p', b' :: String
p' = "x = 10"
b' = "while x > 0:\n  x = x - 1"

setup :: String -> String -> Window -> IO ()
setup p b window = void $ do
  let states = snd $ test p b
  index <- newIORef (1 :: Int)
  let step () = do
        i <- readIORef index
        rerenderState window (states !! i)
        writeIORef index (i + 1)
  _ <- return window # set title "ProgViz"
  rerenderState window (head states)
  body' <- getBody window
  UI.register (E.click body') step

list :: Show a => [a] -> IO Element
list ls = foldl go (UI.ul #. "code-list") ls
  where go ul elt = ul #+ [UI.li #+ [string $ show elt]]

label :: String -> IO Element -> IO Element
label str elt = UI.div #. "label" #+ [UI.span #. "label" #+ [string str], elt]

render :: Value -> IO Element
render (Num n)           = UI.div #. "value" #+ [string $ show n]
render (Str s)           = UI.div #. "value" #+ [string s]
render (Bool b)          = UI.div #. "value" #+ [string $ show b]
render (List ls)         = list ls
render (Node idn _ _)    = UI.div #. "node"  #+ [string $ show idn]
render (Graph _ nodes edges) = UI.div #. "graph" #+ [serialize "fst" $ fst <$> edges,
                                                 serialize "snd" $ snd <$> edges,
                                                 marked nodes]
  where serialize which idns =
          UI.span #. printf "graph-%s" which #+ [string $ show idns]
        marked ns =
          let ids = map (\ (Node i _ _) -> i) $ filter (\ (Node _ _ b) -> b) ns in
          UI.div #. "msg" #+ [string $ show ids]

renderLabeled :: (String, Value) -> IO Element
renderLabeled (_, val@Graph {}) = render val
renderLabeled (str, val)        = label (str ++ ": ") $ render val

renderState :: State -> [IO Element]
renderState = map renderLabeled . M.toList

rerenderState :: Window -> State -> IO ()
rerenderState window newState = void $ do
  clear window
  UI.addStyleSheet window "main.css"
  _ <- getBody window #+ renderState newState
  debug window "foobar"
