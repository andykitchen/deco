\begin{code}
module Main (main) where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad.CC

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
loadIO path = runProgram defaultBindings (load path)

replIO :: IO ()
replIO = (runProgramState defaultBindings . runInputT defaultSettings) repl


load :: FilePath -> CCT r ProgramState ()
load path = readFile' path >>= parse' >>= evaluate >> return ()

repl :: InputT ProgramState ()
repl = do
  input <- getInputLine "> "
  case input of
    Just line -> parse' line >>= evaluate' >>= print' >> repl
    Nothing   -> return ()

evaluate' :: Expr -> InputT ProgramState Value
evaluate' expr = do
  let state = runCCT (evaluate expr)
  lift state

parse'    :: MonadIO    m => String   -> m Expr
print'    :: (MonadIO m, Show a) => a -> m ()
readFile' :: MonadIO    m => FilePath -> m String

parse'    = liftIO . parseStr
print'    = liftIO . print
readFile' = liftIO . readFile
\end{code}
