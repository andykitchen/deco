module Lexer where

import qualified Text.Parsec.Token as P
import Text.Parsec.Token (GenLanguageDef(..))

import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, letter, alphaNum)

opChar :: Parser Char
opChar = oneOf ":!#$%&*+./<=>?@\\^|-~"

language = LanguageDef {
    commentStart = "",
    commentEnd = "",
    commentLine = "#",
    nestedComments = False,
    identStart = letter,
    identLetter = alphaNum,
    opStart = opChar,
    opLetter = opChar,
    reservedNames = [],
    reservedOpNames = [],
    caseSensitive = True
}

lexer :: P.TokenParser ()
lexer = P.makeTokenParser language

whiteSpace = P.whiteSpace lexer
parens     = P.parens lexer
identifier = P.identifier lexer
integer    = P.integer lexer
reservedOp = P.reservedOp lexer