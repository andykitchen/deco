module Main (main) where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO, liftIO)

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


loadIO :: FilePath -> IO ()
loadIO = runProgram defaultBindings . load

replIO :: IO ()
replIO = (runProgram defaultBindings . runInputT defaultSettings) repl


load :: FilePath -> ProgramEnv ()
load path = readFile' path >>= parse' >>= evaluate >> return ()

repl :: InputT ProgramEnv ()
repl = do
  input <- getInputLine "> "
  case input of
    Just line -> parse' line >>= evaluate' >>= print' >> repl
    Nothing   -> return ()


parse'    :: MonadIO    m => String   -> m Expr
evaluate' :: MonadTrans t => Expr     -> t ProgramEnv Value
print'    :: (MonadIO m, Show a) => a -> m ()
readFile' :: MonadIO    m => FilePath -> m String

parse'    = liftIO . parseStr
evaluate' = lift   . evaluate
print'    = liftIO . print
readFile' = liftIO . readFile
