module Lexer where

import qualified Text.Parsec.Token as P
import Text.Parsec.Token (GenLanguageDef(..))

import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, letter, alphaNum)

opChar :: Parser Char
opChar = oneOf ",:;!#$%&*+./<=>?@\\^|-~="

language = LanguageDef {
    commentStart = "",
    commentEnd = "",
    commentLine = "#",
    nestedComments = False,
    identStart = letter,
    identLetter = alphaNum,
    opStart = opChar,
    opLetter = opChar,
    reservedNames = ["if", "else", "true", "false"],
    reservedOpNames =
      ["+", "-", "*", "/", "++", "--",
       "<", ">", "<=", ">=", "==", "!=", "=",
       "&&", "||", "!"],
    caseSensitive = True
}

lexer :: P.TokenParser ()
lexer = P.makeTokenParser language

whiteSpace = P.whiteSpace lexer
parens     = P.parens lexer
braces     = P.braces lexer
semi       = P.semi lexer
identifier = P.identifier lexer
symbol     = P.symbol lexer
integer    = P.natural lexer
float      = P.float lexer
naturalOrFloat = P.naturalOrFloat lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
stringLiteral = P.stringLiteral lexer
