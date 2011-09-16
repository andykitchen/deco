{-# LANGUAGE NoMonomorphismRestriction #-}
module Main (main) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

import System.Environment (getArgs)
import System.Console.Haskeline

import Parser
import Evaluate
import Primitives

main :: IO ()
main = do
     args <- getArgs
     case length args >= 1 of
       True  -> loadIO (head args)
       False -> replIO


loadIO :: String -> IO ()
loadIO = runProgram defaultBindings . load

replIO :: IO ()
replIO = (runProgram defaultBindings . runInputT defaultSettings) repl


load :: String -> ProgramEnv ()
-- load path = readFile' path >>= parse' >>= evaluate >> return ()
load = undefined

repl :: InputT ProgramEnv ()
repl = do
  input <- getInputLine "> "
  case input of
    Just line -> parse' line >>= evaluate' >>= print' >> repl
    Nothing   -> return ()

parse'    = liftIO . parseStr
evaluate' = lift   . evaluate
print'    = liftIO . print
readFile' = liftIO . readFile