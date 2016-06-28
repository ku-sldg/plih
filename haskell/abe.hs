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


-- Type Derivation Function

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

-- Interpreter

interp :: String -> ABE
interp e = let p=(parseABE e) in
           let t=typeof p in
             if ((t==TBool) || (t==TNum))
             then (eval p)
             else error "This should never happen"

-- Alternative Type Derivation Function

typeof' :: ABE -> Either TABE String
typeof' (Num x) = (Left TNum)
typeof' (Plus l r) = let l' = (typeof' l)
                         r' = (typeof' r)
                     in if l'==(Left TNum) && r'==(Left TNum)
                        then (Left TNum)
                        else Right "Type Mismatch in +"
typeof' (Minus l r) = let l' = (typeof' l)
                          r' = (typeof' r)
                      in if l'==(Left TNum) && r'==(Left TNum)
                         then (Left TNum)
                         else Right "Type Mismatch in -"
typeof' (Boolean b) = (Left TBool)
typeof' (And l r) = if (typeof' l) == (Left TBool) && (typeof' r) == (Left TBool)
                    then (Left TBool)
                    else Right "Type mismatch in &&"
typeof' (Leq l r) = if (typeof' l) == (Left TNum) && (typeof' r) == (Left TNum)
                    then (Left TBool)
                    else Right "Type mismatch in <="
typeof' (IsZero v) = if (typeof' v) == (Left TNum)
                     then (Left TBool)
                     else Right "Type mismatch in IsZero"
typeof' (If c t e) = if (typeof' c) == (Left TBool)
                        && (typeof' t)==(typeof' e)
                     then (typeof' t)
                     else Right "Type mismatch in if"

-- Alternative Interpreter Function

interp' :: String -> ABE
interp' e = let p=(parseABE e) in
            case (typeof' p) of
              (Left _) -> eval p
              (Right m) -> error m

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
testParser n = quickCheckWith stdArgs {maxSuccess=n} (\t -> parseABE (pprint t) == t)

testEval :: Int -> IO ()
testEval n = quickCheckWith stdArgs {maxSuccess=n} (\t -> eval (parseABE (pprint t)) == (eval t))

testTypeof :: Int -> IO ()
testTypeof n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> let ty = typeof t
         in ty == TNum || ty == TBool)

testTypeof' :: Int -> IO ()
testTypeof' n = quickCheckWith stdArgs {maxSuccess=n}
  (\t-> case typeof' t of
      (Left _) -> True
      (Right _) -> True)

testEval' :: Int -> IO ()
testEval' n = quickCheckWith stdArgs {maxSuccess=n}
              (\t -> case typeof' t of
                       (Left _) -> eval (parseABE (pprint t)) == (eval t)
                       (Right _) -> True)
