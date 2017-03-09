{-# LANGUAGE GADTs #-}

module Proj2Utils where

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
-- Simple caculator with variables extended Booleans and both static and
-- dynamic type checking.
--
-- Author: Perry Alexander
-- Date: Wed Jul 13 11:24:46 CDT 2016
--
-- Source files for the Boolean Binding Arithmetic Expressions (BBAE)
-- language from PLIH
--

-- BBAE AST Definition

data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: String -> BBAE -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  Seq :: BBAE -> BBAE -> BBAE
  Print :: BBAE -> BBAE
  Cons :: BBAE -> BBAE -> BBAE
  First :: BBAE -> BBAE
  Rest :: BBAE -> BBAE
  IsEmpty :: BBAE -> BBAE
  Empty :: BBAE
  deriving (Show,Eq)

-- Parser

expr :: Parser BBAE
expr = buildExpressionParser opTable term

opTable = [ [ inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft ]
          , [ inFix "<=" Leq AssocLeft
            , preFix "isZero" IsZero ]
          , [ inFix "&&" And AssocLeft ]
          ]

numExpr :: Parser BBAE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

identExpr :: Parser BBAE
identExpr = do i <- identifier lexer
               return (Id i)

bindExpr :: Parser BBAE
bindExpr = do reserved lexer "bind"
              i <- identifier lexer
              reservedOp lexer "="
              v <- expr
              reserved lexer "in"
              e <- expr
              return (Bind i v e)

trueExpr :: Parser BBAE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser BBAE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)

ifExpr :: Parser BBAE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)
            
seqExpr :: Parser BBAE
seqExpr = do reserved lexer "seq"
             f <- expr
             s <- expr
             return (Seq f s)

printExpr :: Parser BBAE
printExpr = do reserved lexer "print"
               t <- expr
               return (Print t)

consExpr :: Parser BBAE
consExpr = do reserved lexer "cons"
              f <- expr
              s <- expr
              return (Cons f s)

firstExpr :: Parser BBAE
firstExpr = do reserved lexer "first"
               t <- expr
               return (First t)
             
restExpr :: Parser BBAE
restExpr = do reserved lexer "rest"
              t <- expr
              return (Rest t)

isEmptyExpr :: Parser BBAE
isEmptyExpr = do reserved lexer "isEmpty"
                 t <- expr
                 return (IsEmpty t)

emptyExpr :: Parser BBAE
emptyExpr = do reserved lexer "empty"
               return Empty
             
term = parens lexer expr
       <|> numExpr
       <|> identExpr
       <|> bindExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr
       <|> consExpr
       <|> firstExpr
       <|> restExpr              
       <|> isEmptyExpr
       <|> emptyExpr
       <|> printExpr
       <|> seqExpr
       
-- Parser invocation

parseBBAE = parseString expr

parseBBAEFile = parseFile expr