module Main (main) where

import Control.Monad.Identity
import Control.Monad.IO.Class

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.String

import System.Console.Haskeline

data Sexp = Cons [Sexp] | Sym String | Num Int
  deriving Show

symbol :: Parser Sexp
symbol = (many1 rune >>= return . Sym) <?> "Intentifier"

rune :: Parser Char
rune = alphaNum <|> oneOf "!#$%&|*+-/:&lt;=>?@^_~."

number :: Parser Sexp
number = try rule <?> "Number"
  where rule = do
          str <- many1 digit
          notFollowedBy rune
          return $ Num (read str)

atom :: Parser Sexp
atom = number <|> symbol

sexp :: Parser Sexp
sexp = do
    char '('
    xs <- sepBy1 sexp ws
    char ')'
    return $ Cons xs
  <|> atom

ws = many1 space

main :: IO ()
main = runInputT defaultSettings repl

repl :: InputT IO ()
repl = do
  input <- getInputLine "> "
  case input of
     Nothing   -> return ()
     Just line -> parseInput sexp line >> repl

parseInput p input
    = case parse p "" input of
        Left err -> do printOutput "parse error"
                       printOutput err
        Right x  -> printOutput (show x)

printOutput :: (MonadIO m, Show a) => a -> InputT m ()
printOutput = outputStrLn . show
