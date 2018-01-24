{-# LANGUAGE GADTs, FlexibleContexts #-}

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
  If0 :: AE -> AE -> AE -> AE
  deriving (Show,Eq)

-- Lift a function over integers into Num

liftNum :: (Int -> Int -> Int) -> AE -> AE -> AE
liftNum f (Num l) (Num r) = (Num (f l r))

-- AST Pretty Printer
-- Prints concrete syntax from abstract syntax

pprintAE :: AE -> String
pprintAE (Num n) = show n
pprintAE (Plus n m) = "(" ++ pprintAE n ++ "+" ++ pprintAE m ++ ")"
pprintAE (Minus n m) = "(" ++ pprintAE n ++ "-" ++ pprintAE m ++ ")"
pprintAE (Mult n m) = "(" ++ pprintAE n ++ "*" ++ pprintAE m ++ ")"
pprintAE (Div n m) = "(" ++ pprintAE n ++ "/" ++ pprintAE m ++ ")"
pprintAE (If0 c t e) = "(if0 " ++ pprintAE c ++ " then " ++ pprintAE t ++ " else " ++ pprintAE e ++ ")"

-- Parser (Requires ParserUtils and Parsec)

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = [ "if0"
                              , "then"
                              , "else"
                              ]
            , reservedOpNames = [ "+","-","*","/"]
            }
  
lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

parseFile p file =
  do program <- readFile file
     case parse p "" program of
       Left e -> print e >> fail "parse error"
       Right r -> return r

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [
  [ inFix "*" Mult AssocLeft
    , inFix "/" Div AssocLeft ]
  , [ inFix "+" Plus AssocLeft
  , inFix "-" Minus AssocLeft ]
  ]
  
numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

ifExpr :: Parser AE
ifExpr  = do reserved lexer "if0"
             c <- expr
             reserved lexer "then"
             t <- expr
             reserved lexer "else"
             e <- expr
             return (If0 c t e)
                     

term = parens lexer expr
       <|> numExpr
       <|> ifExpr

-- Parser invocation
-- Call parseAE to parse a string into the AE data structure.  Call
-- parseAEFile to parse a comlete file.

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
                      if v2==(Num 0) then Nothing else return (liftNum div v1 v2)
eval (If0 t1 t2 t3) = do v1 <- (eval t1)
                         (if (v1 == (Num 0)) then (eval t2) else (eval t3))

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

genMult n =
  do s <- genAE n
     t <- genAE n
     return (Minus s t)

genDiv n =
  do s <- genAE n
     t <- genAE n
     return (Mult s t)

genIf n =
  do c <- genAE n
     t <- genAE n
     e <- genAE n
     return (If0 c t e)

genAE :: Int -> Gen AE
genAE 0 =
  do term <- genNum
     return term
genAE n =
  do term <- oneof [genNum
                   , (genPlus (n-1))
                   , (genMinus (n-1))
                   , (genMult (n-1))
                   , (genDiv (n-1))
                   , (genIf (n-1))]
     return term

-- QuickCheck 

testParseAE :: Int -> IO ()
testParseAE n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> parseAE (pprintAE t) == t)

testInterpAE :: Int -> IO ()
testInterpAE n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interp $ pprintAE t) == (eval t))

