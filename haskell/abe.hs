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
-- Simple calculator language extended with no identifiers extended
-- with Booleans
--
-- Author: Perry Alexander
-- Date: Mon Jun 27 20:16:55 CDT 2016
--
-- Source files for the Arithmetic Boolean Expressions (ABE) language
-- from PLIH
--

-- AST Definition

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

-- AST Pretty Printer

pprint :: ABE -> String
pprint (Num n) = show n
pprint (Boolean b) = show b
pprint (Plus n m) = "(" ++ pprint n ++ " + " ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ " - " ++ pprint m ++ ")"
pprint (And n m) = "(" ++ pprint n ++ " && " ++ pprint m ++ ")"
pprint (Leq n m) = "(" ++ pprint n ++ " <= " ++ pprint m ++ ")"
pprint (IsZero m) = "(isZero " ++ pprint m ++ ")"
pprint (If c n m) = "(if " ++ pprint c ++ " then " ++ pprint n ++ " else " ++ pprint m ++ ")"


-- Parser (Requires ParserUtils and Parsec)

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

parseABEM = parseM expr

parseABE = parseString expr

parseABEFile = parseFile expr

-- Evaluation Function

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

-- Interpreter

interp = eval . parseABE

-- Evaluator with Dynamic Error Checking

evalErr :: ABE -> Either String ABE
evalErr (Num x) = (Right (Num x))
evalErr (Plus t1 t2) =
  let r1 = (evalErr t1)
      r2 = (evalErr t2)
  in case r1 of
       (Left m) -> r1
       (Right (Num v1)) -> case r2 of
                            (Left m) -> r2
                            (Right (Num v2)) -> (Right (Num (v1+v2)))
                            (Right _) -> (Left "Type Error in +")
       (Right _) -> (Left "Type Error in +")
evalErr (Minus t1 t2) = 
  let r1 = (evalErr t1)
      r2 = (evalErr t2)
  in case r1 of
       (Left m) -> r1
       (Right (Num v1)) -> case r2 of
                            (Left m) -> r2
                            (Right (Num v2)) -> (Right (Num (v1-v2)))
                            (Right _) -> (Left "Type Error in -")
       (Right _) -> (Left "Type Error in -")
evalErr (Boolean b) = (Right (Boolean b))
evalErr (And t1 t2) =
  let r1 = (evalErr t1)
      r2 = (evalErr t2)
  in case r1 of
       (Left m) -> r1
       (Right (Boolean v1)) -> case r2 of
                                (Left m) -> r2
                                (Right (Boolean v2)) -> (Right (Boolean (v1 && v2)))
                                (Right _) -> (Left "Type Error in &&")
       (Right _) -> (Left "Type Error in &&")
evalErr (Leq t1 t2) = 
  let r1 = (evalErr t1)
      r2 = (evalErr t2)
  in case r1 of
       (Left m) -> r1
       (Right (Num v1)) -> case r2 of
                            (Left m) -> r2
                            (Right (Num v2)) -> (Right (Boolean (v1 <= v2)))
                            (Right _) -> (Left "Type Error in <=")
       (Right _) -> (Left "Type Error in <=")
evalErr (IsZero t) =
  let r = (evalErr t)
  in case r of
       (Left m) -> r
       (Right (Num v)) -> (Right (Boolean (v == 0)))
       (Right _) -> (Left "Type error in isZero")
evalErr (If t1 t2 t3) =
  let r = (evalErr t1)
  in case r of
       (Left _) -> r
       (Right (Boolean v)) -> if v then (evalErr t2) else (evalErr t3)
       (Right _) -> (Left "Type error in if")

-- Interpreter

interpErr = evalErr . parseABE

testEvalErr :: Int -> IO ()
testEvalErr n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interpErr $ pprint t) == (evalErr t))

testEvals :: Int -> IO ()
testEvals n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (let r = (evalErr t) in
            case r of
              (Right v) -> v == (eval t)
              (Left v) -> True))

-- Type Derivation Function

typeof :: ABE -> Either String TABE
typeof (Num x) = (Right TNum)
typeof (Plus l r) = let l' = (typeof l)
                        r' = (typeof r)
                    in if l'==(Right TNum) && r'==(Right TNum)
                       then (Right TNum)
                       else Left "Type Mismatch in +"
typeof (Minus l r) = let l' = (typeof l)
                         r' = (typeof r)
                     in if l'==(Right TNum) && r'==(Right TNum)
                        then (Right TNum)
                        else Left "Type Mismatch in -"
typeof (Boolean b) = (Right TBool)
typeof (And l r) = if (typeof l) == (Right TBool) && (typeof r) == (Right TBool)
                   then (Right TBool)
                   else Left "Type mismatch in &&"
typeof (Leq l r) = if (typeof l) == (Right TNum) && (typeof r) == (Right TNum)
                   then (Right TBool)
                   else Left "Type mismatch in <="
typeof (IsZero v) = if (typeof v) == (Right TNum)
                    then (Right TBool)
                    else Left "Type mismatch in IsZero"
typeof (If c t e) = if (typeof c) == (Right TBool)
                       && (typeof t)==(typeof e)
                    then (typeof t)
                    else Left "Type mismatch in if"

-- Alternative Interpreter Function

interpTyped :: String -> Either String ABE
interpTyped e = let p=(parseABE e) in
                  case (typeof p) of
                    (Right _) -> (Right (eval p))
                    (Left m) -> (Left m)

-- Testing (Requires QuickCheck 2)

-- Arbitrary AST Generator

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
  do term <- oneof [genNum,(genPlus (n-1))
                   ,(genMinus (n-1))
                   ,(genAnd (n-1))
                   ,(genLeq (n-1))
                   ,(genIsZero (n-1))
                   ,(genIf (n-1))]
     return term

-- QuickCheck 

testParser :: Int -> IO ()
testParser n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> parseABE (pprint t) == t)

testEval :: Int -> IO ()
testEval n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interp $ pprint t) == (eval t))

testTypeof :: Int -> IO ()
testTypeof n = quickCheckWith stdArgs {maxSuccess=n}
  (\t-> case typeof t of
      (Right _) -> True
      (Left _) -> True)

testTypedEval :: Int -> IO ()
testTypedEval n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> case typeof t of
           (Right _) -> eval (parseABE (pprint t)) == (eval t)
           (Left _) -> True)

eqInterp :: Either String ABE -> Either String ABE -> Bool
eqInterp s t =
  case s of
    (Right x) -> case t of
                  (Right y) -> x == y
                  (Left _) -> False
    (Left x) -> case t of
                   (Right y) -> False
                   (Left _) -> True

testTypedErrEval :: Int -> IO ()
testTypedErrEval n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> let t' = pprint t in (eqInterp (interpTyped t') (interpErr t')))

testErrThenTyped :: Int -> IO ()
testErrThenTyped n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> let t' = pprint t in
           case (interpErr t') of
             (Right v) -> (Right v) == interpTyped t'
             (Left _) -> True)
               
testTypedThenErr :: Int -> IO ()
testTypedThenErr n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> let t' = pprint t in
           case (interpTyped t') of
             (Right v) -> (Right v) == interpErr t'
             (Left _) -> True)

