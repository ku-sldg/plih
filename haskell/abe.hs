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
-- Calculator language extended with Booleans
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

-- Evaluator with Dynamic Error 

evalErr :: ABE -> Either ABE String
evalErr (Num x) = (Left (Num x))
evalErr (Plus t1 t2) =
  let r1 = (evalErr t1)
      r2 = (evalErr t2)
  in case r1 of
       (Right m) -> r1
       (Left (Num v1)) -> case r2 of
                            (Right m) -> r2
                            (Left (Num v2)) -> (Left (Num (v1+v2)))
                            (Left _) -> (Right "Type Error in +")
       (Left _) -> (Right "Type Error in +")
evalErr (Minus t1 t2) = 
  let r1 = (evalErr t1)
      r2 = (evalErr t2)
  in case r1 of
       (Right m) -> r1
       (Left (Num v1)) -> case r2 of
                            (Right m) -> r2
                            (Left (Num v2)) -> (Left (Num (v1-v2)))
                            (Left _) -> (Right "Type Error in -")
       (Left _) -> (Right "Type Error in -")
evalErr (Boolean b) = (Left (Boolean b))
evalErr (And t1 t2) =
  let r1 = (evalErr t1)
      r2 = (evalErr t2)
  in case r1 of
       (Right m) -> r1
       (Left (Boolean v1)) -> case r2 of
                                (Right m) -> r2
                                (Left (Boolean v2)) -> (Left (Boolean (v1 && v2)))
                                (Left _) -> (Right "Type Error in &&")
       (Left _) -> (Right "Type Error in &&")
evalErr (Leq t1 t2) = 
  let r1 = (evalErr t1)
      r2 = (evalErr t2)
  in case r1 of
       (Right m) -> r1
       (Left (Num v1)) -> case r2 of
                            (Right m) -> r2
                            (Left (Num v2)) -> (Left (Boolean (v1 <= v2)))
                            (Left _) -> (Right "Type Error in <=")
       (Left _) -> (Right "Type Error in <=")
evalErr (IsZero t) =
  let r = (evalErr t)
  in case r of
       (Right m) -> r
       (Left (Num v)) -> (Left (Boolean (v == 0)))
       (Left _) -> (Right "Type error in isZero")
evalErr (If t1 t2 t3) =
  let r = (evalErr t1)
  in case r of
       (Right _) -> r
       (Left (Boolean v)) -> if v then (evalErr t2) else (evalErr t3)
       (Left _) -> (Right "Type error in if")

-- Interpreter

interpErr = evalErr . parseABE

testEvalErr :: Int -> IO ()
testEvalErr n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interpErr $ pprint t) == (evalErr t))

testEvals :: Int -> IO ()
testEvals n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (let r = (evalErr t) in
            case r of
              (Left v) -> v == (eval t)
              (Right v) -> True))

-- Type Derivation Function

typeof :: ABE -> Either TABE String
typeof (Num x) = (Left TNum)
typeof (Plus l r) = let l' = (typeof l)
                        r' = (typeof r)
                    in if l'==(Left TNum) && r'==(Left TNum)
                       then (Left TNum)
                       else Right "Type Mismatch in +"
typeof (Minus l r) = let l' = (typeof l)
                         r' = (typeof r)
                     in if l'==(Left TNum) && r'==(Left TNum)
                        then (Left TNum)
                        else Right "Type Mismatch in -"
typeof (Boolean b) = (Left TBool)
typeof (And l r) = if (typeof l) == (Left TBool) && (typeof r) == (Left TBool)
                   then (Left TBool)
                   else Right "Type mismatch in &&"
typeof (Leq l r) = if (typeof l) == (Left TNum) && (typeof r) == (Left TNum)
                   then (Left TBool)
                   else Right "Type mismatch in <="
typeof (IsZero v) = if (typeof v) == (Left TNum)
                    then (Left TBool)
                    else Right "Type mismatch in IsZero"
typeof (If c t e) = if (typeof c) == (Left TBool)
                       && (typeof t)==(typeof e)
                    then (typeof t)
                    else Right "Type mismatch in if"

-- Alternative Interpreter Function

interpTyped :: String -> Either ABE String
interpTyped e = let p=(parseABE e) in
                  case (typeof p) of
                    (Left _) -> (Left (eval p))
                    (Right m) -> (Right m)

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
      (Left _) -> True
      (Right _) -> True)

testTypedEval :: Int -> IO ()
testTypedEval n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> case typeof t of
           (Left _) -> eval (parseABE (pprint t)) == (eval t)
           (Right _) -> True)

eqInterp :: Either ABE String -> Either ABE String -> Bool
eqInterp s t =
  case s of
    (Left x) -> case t of
                  (Left y) -> x == y
                  (Right _) -> False
    (Right x) -> case t of
                   (Left y) -> False
                   (Right _) -> True

testTypedErrEval :: Int -> IO ()
testTypedErrEval n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> let t' = pprint t in (eqInterp (interpTyped t') (interpErr t')))

testErrThenTyped :: Int -> IO ()
testErrThenTyped n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> let t' = pprint t in
           case (interpErr t') of
             (Left v) -> (Left v) == interpTyped t'
             (Right _) -> True)
               
testTypedThenErr :: Int -> IO ()
testTypedThenErr n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> let t' = pprint t in
           case (interpTyped t') of
             (Left v) -> (Left v) == interpErr t'
             (Right _) -> True)

