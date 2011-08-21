module Main (main) where

import System.Console.Haskeline

import Parser
import Evaluate

main :: IO ()
main = runInputT defaultSettings repl

repl :: InputT IO ()
repl = do
  input <- getInputLine "> "
  case input of
     Nothing   -> return ()
     Just line -> parseInput line >> repl
