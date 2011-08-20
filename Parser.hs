module Parser (parseInput) where

import Text.Parsec ((<|>), (<?>), try, eof, many1)
import Text.Parsec.Combinator (optional, sepBy, sepEndBy1)
import Text.Parsec.Prim (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))

import Control.Monad.IO.Class (MonadIO)

import System.Console.Haskeline

import Lexer

data Expr = BinOp String Expr Expr
          | UniOp String Expr
          | IntLit Integer
          | StrLit String
          | Ident String
          | Lambda [String] Expr
          | Application Expr [Expr]
          | Multi [Expr]
          | If Expr Expr
  deriving Show

expr        :: Parser Expr
primary     :: Parser Expr
lambda      :: Parser Expr
application :: Parser Expr
multi       :: Parser Expr
if'         :: Parser Expr

asIntLit = return . IntLit
asStrLit = return . StrLit
asIdent  = return . Ident
asMulti  = return . Multi

expr    =  buildExpressionParser table primary
       <?> "expression"

table   = [[prefix "-"],
           [postfix "++", postfix "--"],
           [binary "*"  AssocLeft, binary "/"  AssocLeft],
           [binary "+"  AssocLeft, binary "-"  AssocLeft],
           [binary "==" AssocLeft, binary "!=" AssocLeft,
            binary "<"  AssocLeft, binary ">"  AssocLeft,
            binary "<=" AssocLeft, binary ">=" AssocLeft,
            prefix "!"],
           [binary "=" AssocRight]]

binary  name = Infix   (exprRule name BinOp)
prefix  name = Prefix  (exprRule name UniOp)
postfix name = Postfix (exprRule name UniOp)

exprRule name f  =  reservedOp name >> return (f name)
                <?> "operator"

primary  =  try application
        <|> parens expr
        <|> multi
        <|> (stringLiteral >>= asStrLit)
        <|> if'
        <|> (identifier >>= asIdent)
        <|> (integer >>= asIntLit)
        <|> lambda
        <?> "simple expression"

lambda = do
  symbol "\\"
  args <- many1 identifier
  symbol "->"
  e <- expr
  return (Lambda args e)

application = do
  func <- parens expr <|> (identifier >>= asIdent)
  let argList = sepBy expr (symbol ",")
  args <- parens argList
  return (Application func args)

multi = braces $ sepEndBy1 expr semi >>= asMulti

if' = do
  reserved "if"
  e <- expr
  m <- multi
  return (If e m)


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
        Left err -> do outputStrLn "parse error"
                       printOutput err
        Right x  -> printOutput x

printOutput :: (MonadIO m, Show a) => a -> InputT m ()
printOutput = outputStrLn . show
