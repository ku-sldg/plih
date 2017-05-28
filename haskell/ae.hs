{-# LANGUAGE GADTs #-}

-- Imports for QuickCheck
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

-- Imports for PLIH
import ParserUtils

--
-- Simple caculator over naturals with no identifiers
--
-- Author: Perry Alexander
-- Date: Mon Jun 27 13:34:57 CDT 2016
--
-- Source files for the Arithmetic Expressions (AE) language from PLIH
--

-- AST Definition

data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  deriving (Show,Eq)

-- Lift a function over integers into Num

liftNum :: (Int -> Int -> Int) -> AE -> AE -> AE
liftNum f (Num l) (Num r) = (Num (f l r))

-- AST Pretty Printer

pprintAE :: AE -> String
pprintAE (Num n) = show n
pprintAE (Plus n m) = "(" ++ pprintAE n ++ "+" ++ pprintAE m ++ ")"
pprintAE (Minus n m) = "(" ++ pprintAE n ++ "-" ++ pprintAE m ++ ")"

--instance Show AE where
--  show = pprintAE

-- Parser (Requires ParserUtils and Parsec)

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

eval :: AE -> Maybe AE
eval (Num x) = return (Num x)
eval (Plus t1 t2) = do v1 <- (eval t1)
                       v2 <- (eval t2)
                       return (liftNum (+) v1 v2)
eval (Minus t1 t2) = do v1 <- (eval t1)
                        v2 <- (eval t2)
                        return (liftNum (-) v1 v2)
eval (Mult t1 t2) = do v1 <- (eval t1)
                       v2 <- (eval t2)
                       return (liftNum (*) v1 v2)
eval (Div t1 t2) = do v1 <- (eval t1)
                      v2 <- (eval t2)
                      return (liftNum div v1 v2)

-- Interpreter = parse + eval

interp = eval . parseAE

-- Testing (Requires QuickCheck 2)

-- Arbitrary AST Generator

instance Arbitrary AE where
  arbitrary =
    sized $ \n -> genAE ((rem n 10) + 10)

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

testParseAE :: Int -> IO ()
testParseAE n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> parseAE (pprintAE t) == t)

testInterp :: Int -> IO ()
testInterp n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interp $ pprintAE t) == (eval t))

