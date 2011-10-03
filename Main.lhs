\begin{code}
module Main (main) where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad.CC (CCT, runCCT)

import System.Environment (getArgs)
import System.Console.Haskeline
  (InputT, runInputT, defaultSettings, getInputLine)

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
replIO = runProgram defaultBindings (runInputT defaultSettings repl)


load :: FilePath -> ProgramEnv ()
load path = readFile' path >>= parse' >>= evaluate >> return ()

repl :: InputT (CCT r ProgramState) ()
repl = do
  input <- getInputLine "> "
  case input of
    Just line -> parse' line >>= evaluate' >>= print' >> repl
    Nothing   -> return ()

evaluate' expr = lift (evaluate expr)

parse'    :: MonadIO    m => String   -> m Expr
print'    :: (MonadIO m, Show a) => a -> m ()
readFile' :: MonadIO    m => FilePath -> m String

parse'    = liftIO . parseStr
print'    = liftIO . print
readFile' = liftIO . readFile
\end{code}
