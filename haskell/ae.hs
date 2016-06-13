{-# LANGUAGE GADTs #-}

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Simple caculator with no variables

-- AST

data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  deriving (Show,Eq)

-- Parser

tokenDef =
  javaStyle { Token.reservedOpNames = [ "+","-"] }

lexer = Token.makeTokenParser tokenDef

reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [ [Infix (reservedOp "*" >> return (Mult )) AssocLeft,
               Infix (reservedOp "/" >> return (Div )) AssocLeft ]
            ]

numExpr :: Parser AE
numExpr = do i <- integer
             return (Num (fromInteger i))

term = parens expr <|> numExpr

-- Parser invocation

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

parseAE = parseString expr

parseFile p file =
  do program <- readFile file
     case parse p "" program of
       Left e -> print e >> fail "parse error"
       Right r -> return r

parseAEFile = parseFile expr

-- Calculation Function

calc :: AE -> Int
calc (Num x) = x
calc (Plus l r) = (calc l) + (calc r)
calc (Minus l r) = (calc l) - (calc r)
calc (Mult l r) = (calc l) * (calc r)
calc (Div l r) = div (calc l) (calc r)

-- Interpreter = parse + calc

interp = calc . parseAE
