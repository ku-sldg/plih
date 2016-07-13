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
-- Simple caculator with variables extended with Booleans
--
-- Author: Perry Alexander
-- Date: Wed Jul 13 11:24:46 CDT 2016
--
-- Source files for the Boolean Binding Arithmetic Expressions (BBAE)
-- language from PLIH
--

-- Calculator language extended with variables and Booleans

data TBBAE where
  TNum :: TBBAE
  TBool :: TBBAE
  deriving (Show,Eq)

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

term = parens lexer expr
       <|> numExpr
       <|> identExpr
       <|> bindExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr

parseBAE = parseString expr

parseBAEFile = parseFile expr

-- Parser invocation

parseBBAE = parseString expr

parseBBAEFile = parseFile expr

type Env = [(String,BBAE)]
type Cont = [(String,TBBAE)]
    
calc :: BBAE -> Env -> BBAE
calc (Num x) env = (Num x)
calc (Plus l r) env = let (Num l') = (calc l env)
                          (Num r') = (calc r env)
                      in (Num (l'+r'))
calc (Minus l r) env = let (Num l') = (calc l env)
                           (Num r') = (calc r env)
                       in (Num (l'-r'))
calc (Bind i v b) env = let v' = calc v env in
                          calc b ((i,v'):env)
calc (Id id) env = case (lookup id env) of
                     Just x -> x
                     Nothing -> error "Varible not found"
calc (Boolean b) env = (Boolean b)
calc (And l r) env = let (Boolean l') = (calc l env)
                         (Boolean r') = (calc r env)
                      in (Boolean (l' && r'))
calc (Leq l r) env = let (Num l') = (calc l env)
                         (Num r') = (calc r env)
                      in (Boolean (l' <= r'))
calc (IsZero v) env = let (Num v') = (calc v env)
                      in (Boolean (v' == 0))
calc (If c t e) env = let (Boolean c') = (calc c env)
                      in if c' then (calc t env) else (calc e env)


typeof :: BBAE -> Cont -> TBBAE
typeof (Num x) cont = TNum
typeof (Plus l r) cont = let l' = (typeof l cont)
                             r' = (typeof r cont)
                         in if l'==TNum && r'==TNum
                            then TNum
                            else error "Type Mismatch in +"
typeof (Minus l r) cont = let l' = (typeof l cont)
                              r' = (typeof r cont)
                          in if l'==TNum && r'==TNum
                             then TNum
                             else error "Type Mismatch in -"
typeof (Bind i v b) cont = let v' = typeof v cont in
                             typeof b ((i,v'):cont)
typeof (Id id) cont = case (lookup id cont) of
                        Just x -> x
                        Nothing -> error "Varible not found"
typeof (Boolean b) cont = TBool
typeof (And l r) cont = if (typeof l cont) == TBool && (typeof r cont) == TBool
                        then TBool
                        else error "Type mismatch in &&"
typeof (Leq l r) cont = if (typeof l cont) == TNum && (typeof r cont) == TNum
                        then TBool
                        else error "Type mismatch in <="
typeof (IsZero v) cont = if (typeof v cont) == TNum
                         then TBool
                         else error "Type mismatch in IsZero"
typeof (If c t e) cont = if (typeof c cont) == TBool
                            && (typeof t cont)==(typeof e cont)
                         then (typeof t cont)
                         else error "Type mismatch in if"

eval :: String -> BBAE
eval e = let p=(parseBBAE e) in
           let t=typeof p [] in
             if ((t==TBool) || (t==TNum))
             then (calc p [])
             else error "This should never happen"
