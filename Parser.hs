module Parser (parseStr, Expr(..), Symbol) where

import Text.Parsec ((<|>), (<?>), try, eof, many1)
import Text.Parsec.Combinator (optional, optionMaybe, sepBy, sepEndBy1)
import Text.Parsec.Prim (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))

import Lexer

type Symbol = String

data Expr = BinOp String Expr Expr
          | UniOp String Expr
          | IntLit Integer
          | StrLit String
          | Ident Symbol
          | Lambda [Symbol] Expr
          | Application Expr [Expr]
          | Multi [Expr]
          | If Expr Expr (Maybe Expr)
  deriving (Show, Eq)

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

binary  name assoc = Infix  (exprRule name BinOp) assoc

prefix  name = Prefix  (exprRule name UniOp)
postfix name = Postfix (exprRule name UniOp)

exprRule name f  =  reservedOp name >> return (f name)
                <?> "operator"

primary  =  try application
        <|> parens expr
        <|> multi
        <|> if'
        <|> (stringLiteral >>= asStrLit)
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

multi = braces (sepEndBy1 expr semi >>= asMulti)

if' = do
  reserved "if"
  test  <- expr
  then' <- multi
  else' <- optionMaybe (reserved "else" >> multi)
  return (If test then' else')

program = do
    whiteSpace
    x <- expr
    eof
    return x

parseStr :: String -> IO Expr
parseStr = parseStr' program

parseStr' :: Parser Expr -> String -> IO Expr
parseStr' p input
    = case parse p "" input of
        Left err -> do putStrLn "parse error"
                       print err
                       return (IntLit 0)
        Right ast -> print ast >> return ast
