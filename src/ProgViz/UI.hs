module ProgViz.UI where

import           Control.Monad

import qualified Data.Map                    as M

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

import           Text.Printf

import           ProgViz.Run
import           ProgViz.State               (State)
import           ProgViz.Types

main :: IO ()
main = startGUI defaultConfig {
   tpStatic = Just "src/",
   tpCustomHTML = Just "html/index.html"
  } setup

prelude, body :: String
prelude = "g = Graph.Create('foo', [1,2,1,3,1,4,1,5,5,3,3,6,6,7,7,8,8,5])\nx = 0\nls=[1,2,3]\nls2 = []"
body = "for i in ls:\n  i\n  x = 10 + x + i\n  ls2.add(i * 10)"

setup :: Window -> IO ()
setup window = void $ do
  _ <- return window # set title "ProgViz"
  UI.addStyleSheet window "main.css"
  getBody window #+ (renderState (head . snd $ test prelude body) ++ [mkElement "script" # set (attr "id") "js/raphael.js"])

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
render (Graph _ _ edges) = UI.div #. "graph" #+ [serialize "fst" $ fst <$> edges,
                                                 serialize "snd" $ snd <$> edges]
  where serialize which idns =
          UI.span #. printf "graph-%s" which #+ [string $ show idns]

renderLabeled :: (String, Value) -> IO Element
renderLabeled (_, val@Graph {}) = render val
renderLabeled (str, val)        = label (str ++ ": ") $ render val

renderState :: State -> [IO Element]
renderState = map renderLabeled . M.toList
