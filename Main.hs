module Main (main) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

import System.Console.Haskeline

import Parser
import Evaluate
import Primitives

main :: IO ()
main = (runProgram defaultBindings . runInputT defaultSettings) repl

repl :: InputT ProgramEnv ()
repl = do
  input <- getInputLine "> "
  case input of
    Just line -> parse' line >>= evaluate' >>= print' >> repl
    Nothing   -> return ()

parse'    = liftIO . parseStr
evaluate' = lift   . evaluate
print'    = liftIO . print
