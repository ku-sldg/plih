{-# LANGUAGE GADTs #-}

import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic
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

pprint :: AE -> String
pprint (Num n) = show n
pprint (Plus n m) = "(" ++ pprint n ++ "+" ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ "-" ++ pprint m ++ ")"

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

-- Evaluation Function

eval :: AE -> AE
eval (Num x) = (Num x)
eval (Plus t1 t2) = let (Num v1) = (eval t1)
                        (Num v2) = (eval t2)
                    in (Num (v1+v2))
eval (Minus t1 t2) = let (Num v1) = (eval t1)
                         (Num v2) = (eval t2)
                     in (Num (v1-v2))
eval (Mult t1 t2) = let (Num v1) = (eval t1)
                        (Num v2) = (eval t2)
                    in (Num (v1*v2))
eval (Div t1 t2) = let (Num v1) = (eval t1)
                       (Num v2) = (eval t2)
                   in (Num (div v1 v2))

-- Interpreter = parse + eval

interp = eval . parseAE

-- Generator

instance Arbitrary AE where
  arbitrary =
    sized $ \n -> genAE (rem n 10)

genNum =
  do t <- choose (0,100)
     return (Num t)

genPlus n =
  do s <- genAE n
     t <- genAE n
     return (Plus s t)

genMinus n =
  do s <- genAE n
     t <- genAE n
     return (Minus s t)

genAE :: Int -> Gen AE
genAE 0 = 
  do term <- genNum
     return term
genAE n =
  do term <- oneof [genNum,(genPlus (n-1)),(genMinus (n-1))]
     return term

-- QuickCheck

testParser :: Int -> IO ()
testParser n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> parseAE (pprint t) == t)

testEval :: Int -> IO ()
testEval n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> eval (parseAE (pprint t)) == (eval t))
