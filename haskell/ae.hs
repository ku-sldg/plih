{-# LANGUAGE GADTs #-}

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

import ParserUtils

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

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [ [ inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft ]
            ]

numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

term = parens lexer expr
       <|> numExpr

-- Parser invocation

parseAE = parseString expr

parseAEFile = parseFile expr

-- Calculation Function

calc :: AE -> AE
calc (Num x) = (Num x)
calc (Plus t1 t2) = let (Num v1) = (calc t1)
                        (Num v2) = (calc t2)
                    in (Num (v1+v2))
calc (Minus t1 t2) = let (Num v1) = (calc t1)
                         (Num v2) = (calc t2)
                     in (Num (v1-v2))
calc (Mult t1 t2) = let (Num v1) = (calc t1)
                        (Num v2) = (calc t2)
                    in (Num (v1*v2))
calc (Div t1 t2) = let (Num v1) = (calc t1)
                       (Num v2) = (calc t2)
                   in (Num (div v1 v2))

-- Interpreter = parse + calc

interp = calc . parseAE
