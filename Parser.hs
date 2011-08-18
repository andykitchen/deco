module Parser (parseInput) where

import Text.Parsec ((<|>), (<?>), eof)
import Text.Parsec.Prim (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))

import Control.Monad.IO.Class (MonadIO)

import System.Console.Haskeline

import Lexer

data Expr = BinOp String Expr Expr
          | UniOp String Expr
          | Lit Integer
          | Ident String
  deriving Show

asLit   = return . Lit
asIdent = return . Ident

expr :: Parser Expr
expr    =  buildExpressionParser table primary
       <?> "expression"

table   = [[prefix "-"],
           [postfix "++", postfix "--"],
           [binary "*" AssocLeft, binary "/" AssocLeft],
           [binary "+" AssocLeft, binary "-" AssocLeft]]

binary  name = Infix   (exprRule name BinOp)
prefix  name = Prefix  (exprRule name UniOp)
postfix name = Postfix (exprRule name UniOp)

exprRule name f = (reservedOp name >> return (f name)) <?> "operator"

primary =  parens expr
       <|> (identifier >>= asIdent)
       <|> (integer >>= asLit)
       <?> "simple expression"


parseInput :: MonadIO m => String -> InputT m ()
parseInput = parseInput' expr

parse' p = parse $
  do
    whiteSpace
    x <- p
    eof
    return x

parseInput' :: (MonadIO m, Show a) => Parser a -> String -> InputT m ()
parseInput' p input
    = case parse' p "<stdin>" input of
        Left err -> do printOutput "parse error"
                       printOutput err
        Right x  -> printOutput x

printOutput :: (MonadIO m, Show a) => a -> InputT m ()
printOutput = outputStrLn . show
