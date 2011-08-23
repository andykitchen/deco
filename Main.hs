module Main (main) where

import Control.Monad.IO.Class (liftIO)

import System.Console.Haskeline

import Parser
import Evaluate

main :: IO ()
main = runInputT defaultSettings (repl defaultBindings)

repl :: Bindings -> InputT IO ()
repl env = do
  input <- getInputLine "> "
  case input of
    Nothing   -> return ()
    Just line -> do
      ast  <- parseInput line
      env' <- liftIO $ do
        (value, env') <- evaluateIO ast env
        print value
        return env'
      repl env'
