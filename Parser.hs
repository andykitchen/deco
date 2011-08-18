module Parser (parseInput) where

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.String

import Control.Monad.Identity
import Control.Monad.IO.Class

import System.Console.Haskeline

import Lexer

data Sexp = Cons [Sexp] | Sym String | Num Integer
  deriving Show

asNum  = return . Num
asSym  = return . Sym
asCons = return . Cons

atom :: Parser Sexp
atom = (integer >>= asNum) <|> (identifier >>= asSym)

sexp :: Parser Sexp
sexp = parens (many sexp >>= asCons) <|> atom

parseInput :: MonadIO m => String -> InputT m ()
parseInput = parseInput' sexp

parse' p = parse $
  do
    whiteSpace
    x <- p
    eof
    return x

parseInput' p input
    = case parse' p "<stdin>" input of
        Left err -> do printOutput "parse error"
                       printOutput err
        Right x  -> printOutput x

printOutput :: (MonadIO m, Show a) => a -> InputT m ()
printOutput = outputStrLn . show
