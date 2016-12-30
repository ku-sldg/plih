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

liftNum :: (Int -> Int -> Int) -> ABE -> ABE -> ABE
liftNum f (Num x) (Num y) = (Num (f x y))

liftNum2Bool :: (Int -> Int -> Bool) -> ABE -> ABE -> ABE
liftNum2Bool f (Num x) (Num y) = (Boolean (f x y))

liftBool :: (Bool -> Bool -> Bool) -> ABE -> ABE -> ABE
liftBool f (Boolean x) (Boolean y) = (Boolean (f x y))

evalErr :: ABE -> Maybe ABE
evalErr (Num x) = (Just (Num x))
evalErr (Plus t1 t2) = do
  r1 <- (evalErr t1) ;
  r2 <- (evalErr t2) ;
  Just (liftNum (+) r1 r2)
evalErr (Minus t1 t2) = do
  r1 <- (evalErr t1) ;
  r2 <- (evalErr t2) ;
  Just (liftNum (-) r1 r2)
evalErr (Boolean b) = (Just (Boolean b))
evalErr (And t1 t2) = do
  r1 <- (evalErr t1) ;
  r2 <- (evalErr t2) ;
  Just (liftBool (&&) r1 r2)
evalErr (Leq t1 t2) =  do
  r1 <- (evalErr t1) ;
  r2 <- (evalErr t2) ;
  Just (liftNum2Bool (<=) r1 r2)
evalErr (IsZero t) = do
  r <- (evalErr t)
  Just (liftNum2Bool (==) r (Num 0))
evalErr (If t1 t2 t3) = do
  (Boolean v) <- (evalErr t1)
  (if v then (evalErr t2) else (evalErr t3))

-- Interpreter

interpErr = evalErr . parseABE

testEvalErr :: Int -> IO ()
testEvalErr n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interpErr $ pprint t) == (evalErr t))

testEvals :: Int -> IO ()
testEvals n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (let r = (evalErr t) in
            case r of
              (Just v) -> v == (eval t)
              Nothing -> True))

-- Type Derivation Function

typeof :: ABE -> Maybe TABE
typeof (Num x) = (Just TNum)
typeof (Plus l r) = do
  l' <- (typeof l) ;
  r' <- (typeof r) ;
  if l'== TNum && r'== TNum then (Just TNum) else Nothing
typeof (Minus l r) = do
  l' <- (typeof l) ;
  r' <- (typeof r) ;
  if l'== TNum && r'== TNum then (Just TNum) else Nothing
typeof (Boolean b) = (Just TBool)
typeof (And l r) = do
  l' <- (typeof l) ;
  r' <- (typeof r) ;
  if l'== TBool && r'== TBool then (Just TBool) else Nothing
typeof (Leq l r) = do
  l' <- (typeof l) ;
  r' <- (typeof r) ;
  if l'== TNum && r'== TNum then (Just TBool) else Nothing
typeof (IsZero v) = do
  t' <- (typeof v) ;
  if t'== TNum then (Just TBool) else Nothing
typeof (If c t e) = do
  c' <- (typeof c) ;
  t' <- (typeof t) ;
  e' <- (typeof e) ;
  if c' ==  TBool && t'==e' then (Just t') else Nothing

-- Alternative Interpreter Function

interpTyped :: String -> Maybe ABE
interpTyped e = let p=(parseABE e) in
                  case (typeof p) of
                    (Just _) -> (Just (eval p))
                    Nothing -> Nothing

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
      Just _ -> True
      Nothing -> True)

testTypedEval :: Int -> IO ()
testTypedEval n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> case typeof t of
           (Just _) -> eval (parseABE (pprint t)) == (eval t)
           Nothing -> True)

eqInterp :: Maybe ABE -> Maybe ABE -> Bool
eqInterp s t = s==t

testTypedErrEval :: Int -> IO ()
testTypedErrEval n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> let t' = pprint t in (eqInterp (interpTyped t') (interpErr t')))

testErrThenTyped :: Int -> IO ()
testErrThenTyped n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> let t' = pprint t in
           case (interpErr t') of
             (Just v) -> (Just v) == interpTyped t'
             Nothing -> True)
               
testTypedThenErr :: Int -> IO ()
testTypedThenErr n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> let t' = pprint t in
           case (interpTyped t') of
             (Just v) -> (Just v) == interpErr t'
             Nothing -> True)

-- Alernative Monadic Evaluator (Currently not included in PLIH)

evalM :: ABE -> Either String ABE
evalM (Num x) = (Right (Num x))
evalM (Boolean x) = (Right (Boolean x))
evalM (Plus t1 t2) = do
  t1' <- (evalM t1)
  t2' <- (evalM t2)
  case t1' of
    (Num v1) -> case t2' of
                  (Num v2) -> (Right (Num (v1+v2)))
                  (Boolean _) -> (Left "Type Error in +")
    (Boolean _) -> (Left "Type Error in +")
evalM (Minus t1 t2) = do
  t1' <- (evalM t1)
  t2' <- (evalM t2)
  case t1' of
    (Num v1) -> case t2' of
                  (Num v2) -> (Right (Num (v1-v2)))
                  (Boolean _) -> (Left "Type Error in -")
    (Boolean _) -> (Left "Type Error in -")
evalM (And t1 t2) = do
  t1' <- (evalM t1)
  t2' <- (evalM t2)
  case t1' of
    (Boolean v1) -> case t2' of
                  (Boolean v2) -> (Right (Boolean (v1 && v2)))
                  (Num _) -> (Left "Type Error in &&")
    (Num _) -> (Left "Type Error in &&")
evalM (Leq t1 t2) = do
  t1' <- (evalM t1)
  t2' <- (evalM t2)
  case t1' of
    (Num v1) -> case t2' of
                  (Num v2) -> (Right (Boolean (v1 <= v2)))
                  (Boolean _) -> (Left "Type Error in <=")
    (Boolean _) -> (Left "Type Error in +")
evalM (IsZero t) = do
  t' <- (evalM t)
  case t' of
    (Num v) -> (Right (Boolean (v == 0)))
    (Boolean _) -> (Left "Type Error in isZero")
evalM (If t1 t2 t3) = do
  t1' <- (evalM t1)
  case t1' of
    (Num v) -> (Left "Type Error in if")
    (Boolean v) ->  if v then (evalM t2) else (evalM t3)

interpM = evalM . parseABE

testEvalM :: Int -> IO ()
testEvalM n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interpM $ pprint t) == (evalM t))
