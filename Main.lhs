\begin{code}
module Main (main) where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad.CC (CCT, runCCT)

import System.Environment (getArgs)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine)
import System.Console.Haskeline.IO
  (InputState, initializeInput, closeInput, queryInput, cancelInput)
import Control.Exception (bracketOnError)

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
replIO = bracketOnError
            (initializeInput defaultSettings)
            cancelInput
            (\hd -> replInput hd >> closeInput hd)
         where
          replInput hd = runProgram defaultBindings (repl hd)


load :: FilePath -> ProgramEnv ()
load path = readFile' path >>= parse' >>= evaluate >> return ()

repl :: InputState -> ProgramEnv ()
repl hd = do
    minput <- queryInput' hd (getInputLine "> ")
    case minput of
      Just line -> parse' line >>= evaluate >>= print' >> repl hd
      Nothing   -> return ()


parse'    :: MonadIO    m => String   -> m Expr
print'    :: (MonadIO m, Show a) => a -> m ()
readFile' :: MonadIO    m => FilePath -> m String

queryInput' :: MonadIO m => InputState -> InputT IO a -> m a

parse'    = liftIO . parseStr
print'    = liftIO . print
readFile' = liftIO . readFile

queryInput' hd input = liftIO (queryInput hd input)

\end{code}
