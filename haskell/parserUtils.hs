module ParserUtils where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

-- Parser

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = [ "lambda"
                              , "bind"
                              , "in"
                              , "if"
                              , "then"
                              , "else"
                              , "isZero"
                              , "true"
                              , "false"
                              , "lambda"
                              , "fix"
                              , "app" ]
            , reservedOpNames = [ "+","-","*","/","&&","||","<=","="]
            }
  
lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseM p str = parse p "" str

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

parseFile p file =
  do program <- readFile file
     case parse p "" program of
       Left e -> print e >> fail "parse error"
       Right r -> return r

