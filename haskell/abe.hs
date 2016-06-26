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

-- Calculator language extended with type derivation function

data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving (Show,Eq)

data ABE where
  Num :: Int -> ABE
  Plus :: ABE -> ABE -> ABE
  Minus :: ABE -> ABE -> ABE
  Boolean :: Bool -> ABE
  And :: ABE -> ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
  If :: ABE -> ABE -> ABE -> ABE
  deriving (Show,Eq)
                    
-- Parser

expr :: Parser ABE
expr = buildExpressionParser opTable term

opTable = [ [ inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft ]
            , [ inFix "<=" Leq AssocLeft
              , preFix "isZero" IsZero ]
            , [ inFix "&&" And AssocLeft ]
            ]

numExpr :: Parser ABE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

trueExpr :: Parser ABE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser ABE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)

ifExpr :: Parser ABE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)

term = parens lexer expr
       <|> numExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr

-- Parser invocation

parseABE = parseString expr

parseABEFile = parseFile expr

-- Interpreter

eval :: ABE -> ABE
eval (Num x) = (Num x)
eval (Plus t1 t2) = let (Num v1) = (eval t1)
                        (Num v2) = (eval t2)
                    in (Num (v1+v2))
eval (Minus t1 t2) = let (Num v1) = (eval t1)
                         (Num v2) = (eval t2)
                     in (Num (v1-v2))
eval (Boolean b) = (Boolean b)
eval (And t1 t2) = let (Boolean v1) = (eval t1)
                       (Boolean v2) = (eval t2)
                   in (Boolean (v1 && v2))
eval (Leq t1 t2) = let (Num v1) = (eval t1)
                       (Num v2) = (eval t2)
                   in (Boolean (v1 <= v2))
eval (IsZero t) = let (Num v) = (eval t)
                  in (Boolean (v == 0))
eval (If t1 t2 t3) = let (Boolean v) = (eval t1)
                     in if v then (eval t2) else (eval t3)


typeof :: ABE -> TABE
typeof (Num x) = TNum
typeof (Plus l r) = let l' = (typeof l)
                        r' = (typeof r)
                    in if l'==TNum && r'==TNum
                       then TNum
                       else error "Type Mismatch in +"
typeof (Minus l r) = let l' = (typeof l)
                         r' = (typeof r)
                     in if l'==TNum && r'==TNum
                        then TNum
                        else error "Type Mismatch in -"
typeof (Boolean b) = TBool
typeof (And l r) = if (typeof l) == TBool && (typeof r) == TBool
                   then TBool
                   else error "Type mismatch in &&"
typeof (Leq l r) = if (typeof l) == TNum && (typeof r) == TNum
                   then TBool
                   else error "Type mismatch in <="
typeof (IsZero v) = if (typeof v) == TNum
                    then TBool
                    else error "Type mismatch in IsZero"
typeof (If c t e) = if (typeof c) == TBool
                       && (typeof t)==(typeof e)
                    then (typeof t)
                    else error "Type mismatch in if"

interp :: String -> ABE
interp e = let p=(parseABE e) in
           let t=typeof p in
             if ((t==TBool) || (t==TNum))
             then (eval p)
             else error "This should never happen"

-- QuickCheck

instance Arbitrary ABE where
  arbitrary =
    sized $ \n -> genABE (rem n 10)

genNum =
  do t <- choose (0,100)
     return (Num t)

genBool =
  do t <- choose (True,False)
     return (Boolean t)

genPlus n =
  do s <- genABE n
     t <- genABE n
     return (Plus s t)

genMinus n =
  do s <- genABE n
     t <- genABE n
     return (Minus s t)

genAnd n =
  do s <- genABE n
     t <- genABE n
     return (And s t)

genLeq n =
  do s <- genABE n
     t <- genABE n
     return (Leq s t)

genIsZero n =
  do s <- genABE n
     return (IsZero s)

genIf n =
  do s <- genABE n
     t <- genABE n
     u <- genABE n
     return (If s t u)

genABE :: Int -> Gen ABE
genABE 0 = 
  do term <- oneof [genNum,genBool]
     return term
genABE n =
  do term <- oneof [genNum,(genPlus (n-1)),(genMinus (n-1))]
     return term

pprint :: ABE -> String
pprint (Num n) = show n
pprint (Boolean b) = show b
pprint (Plus n m) = "(" ++ pprint n ++ "+" ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ "-" ++ pprint m ++ ")"
pprint (And n m) = "(" ++ pprint n ++ "&&" ++ pprint m ++ ")"
pprint (Leq n m) = "(" ++ pprint n ++ "<=" ++ pprint m ++ ")"
pprint (IsZero m) = "(isZero" ++ pprint m ++ ")"
pprint (If c n m) = "(if " ++ pprint c ++ " then " ++ pprint m ++ "else" ++ pprint m ++ ")"

testParser :: Int -> IO ()
testParser n = quickCheckWith stdArgs { maxSuccess=n} (\n -> parseABE (pprint n) == n)

testEval :: Int -> IO ()
testEval n = quickCheckWith stdArgs { maxSuccess=n} (\n -> eval (parseABE (pprint n)) == (eval n))

