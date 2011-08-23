module Main (main) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

import System.Console.Haskeline

import Parser
import Evaluate

main :: IO ()
main = (runProgram defaultBindings . runInputT defaultSettings) repl

repl :: InputT ProgramEnv ()
repl = do
  input <- getInputLine "> "
  case input of
    Nothing   -> return ()
    Just line -> do
      ast   <- parse' line
      value <- evaluate' ast
      print' value
      repl

evaluate' = lift   . evaluate
print'    = liftIO . print
parse'    = liftIO . parseStr
