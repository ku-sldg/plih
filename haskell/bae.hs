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
-- Simple caculator with identifiers defined using bind
--
-- Author: Perry Alexander
-- Date: Tue Jul  5 11:41:01 CDT 2016
--
-- Source files for the Binding Arithmetic Expressions (BAE) language from
-- PLIH
--

data BAE where
  Num :: Int -> BAE
  Plus :: BAE -> BAE -> BAE
  Minus :: BAE -> BAE -> BAE
  Bind :: String -> BAE -> BAE -> BAE
  Id :: String -> BAE
  deriving (Show,Eq)

pprint :: BAE -> String
pprint (Num n) = show n
pprint (Id s) = s
pprint (Plus n m) = "(" ++ pprint n ++ "+" ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ "-" ++ pprint m ++ ")"
pprint (Bind n v b) = "(bind " ++ n ++ " = " ++ pprint v ++ " in " ++ pprint b ++ ")"

-- Parser (Requires ParserUtils and Parsec)

expr :: Parser BAE
expr = buildExpressionParser opTable term

opTable = [ [ inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft ]
            ]

numExpr :: Parser BAE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

identExpr :: Parser BAE
identExpr = do i <- identifier lexer
               return (Id i)

bindExpr :: Parser BAE
bindExpr = do reserved lexer "bind"
              i <- identifier lexer
              reservedOp lexer "="
              v <- expr
              reserved lexer "in"
              e <- expr
              return (Bind i v e)

term = parens lexer expr
       <|> numExpr
       <|> identExpr
       <|> bindExpr

parseBAE = parseString expr

parseBAEFile = parseFile expr

-- Substitution

subst :: String -> BAE -> BAE -> BAE
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Bind i' v' b') = if i==i'
                            then (Bind i' (subst i v v') b')
                            else (Bind i' (subst i v v') (subst i v b'))
subst i v (Id i') = if i==i'
                    then v
                    else (Id i')
       
evals :: BAE -> Int
evals (Num x) =  x
evals (Plus l r) = (evals l) +  (evals r)
evals (Minus l r) = (evals l) - (evals r)
evals (Bind i v b) = (evals (subst i (Num (evals v)) b))
evals (Id id) = error "Undeclared Variable"

interps = evals . parseBAE

-- Evaluation

type Env = [(String,Int)]
    
eval :: Env -> BAE -> Int
eval env (Num x) = x
eval env (Plus l r) = (eval env l) + (eval env r)
eval env (Minus l r) = (eval env l) - (eval env r)
eval env (Bind i v b) =
  let v' = eval env v in
    eval ((i,v'):env) b
eval env (Id id) = case (lookup id env) of
                     Just x -> x
                     Nothing -> error "Varible not found"
                                            

interp = (eval []) . parseBAE


-- Testing (Requires QuickCheck 2)

-- Arbitrary AST Generator

instance Arbitrary BAE where
  arbitrary =
    sized $ \n -> genBAE ((rem n 10) + 10) []

genNum =
  do t <- choose (0,100)
     return (Num t)

genPlus n e =
  do s <- genBAE n e
     t <- genBAE n e
     return (Plus s t)

genMinus n e =
  do s <- genBAE n e
     t <- genBAE n e
     return (Minus s t)

genName =
  do i <- choose ('v','z')
     return [i]

genId e =
  do n <- elements e
     return (Id n)

genBind n e =
  do i <- genName
     v <- genBAE n e
     b <- genBAE n (i:e)
     return (Bind i v b)
     
genBAE :: Int -> [String] -> Gen BAE
genBAE 0 e =
  do term <- oneof (case e of
                      [] -> [genNum]
                      _ -> [genNum
                           , (genId e)])
     return term
genBAE n e =
  do term <- oneof [genNum
                   , (genPlus (n-1) e)
                   , (genMinus (n-1) e)
                   , (genBind (n-1) e)]
     return term

-- QuickCheck 

testParser :: Int -> IO ()
testParser n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> parseBAE (pprint t) == t)

testEval :: Int -> IO ()
testEval n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interp $ pprint t) == (eval [] t))

testEvals :: Int -> IO ()
testEvals n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interps $ pprint t) == (evals t))

testCompare :: Int -> IO ()
testCompare n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (eval [] t) == (evals t))
