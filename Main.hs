module Main where

import System.IO
import System.Environment
import Control.Monad

import Evaluator (tableau, initialFormula, getPaths, isValid)


--main :: IO()
--main = 
--   let evalFormula = [initialFormula "(p | (q & r)) > ((p | q) & (p | r))"]
 --      tree = tableau [] evalFormula
--       paths = getPaths $ tree
--       validade = if isValid tree
--                  then "É uma tautologia"
--                  else "É fálsificavel"
--   in putStrLn $ paths ++ "\n\n" ++ validade



main :: IO()
main = do
   args <- getArgs
   forM_ args handleArguments

handleArguments :: String -> IO()
handleArguments arg = do
   fileHandle <- openFile arg ReadMode
   content <- hGetContents fileHandle
   let linesOfFile = lines content
   forM_ linesOfFile executeTableau


executeTableau :: String -> IO()
executeTableau s = do
      putStrLn $ validade ++ "\n\n" ++ paths ++ "\n"
      where
         evalFormula = [initialFormula s]
         tree = tableau [] evalFormula
         paths = getPaths tree
         validade = if isValid tree
                    then "\"" ++ s ++ "\" é uma tautologia"
                    else "\"" ++ s ++"\" é falsificável"
