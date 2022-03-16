module Main where

import Parse
import ParseProg
import System.IO

main :: IO ()
main = do
    e <- parse parseProg <$> readF
    print e

readF :: IO String
readF = do
    inh <- openFile "input.txt" ReadMode
    hGetContents inh

comp :: [(Program Name, Name)] -> Program Name
comp []        = error "No parse"
comp [(e, [])] = e
comp [(_, a)]  = error ("doesnt use all input" ++ a)