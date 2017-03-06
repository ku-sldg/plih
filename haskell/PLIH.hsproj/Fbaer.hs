{-# LANGUAGE GADTs,FlexibleContexts,UndecidableInstances #-}

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

-- Definitions of Fixedpoint (Unused below)

newtype Fix f = In {out :: f (Fix f) }

newtype Any = Any {getAny :: Bool} deriving Show

instance Show (f (Fix f)) => Show (Fix f) where
  show x = "(" ++ show (out x) ++ ")"

instance Eq (f (Fix f)) => Eq (Fix f) where
  a == b = out a == out b

instance Ord (f (Fix f)) => Ord (Fix f) where
  a `compare` b = out a `compare` out b

cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata f = f . fmap (cata f) . out

type Env' a = Fix (L a)

data L a b = NilF | ConsF a b deriving (Show)

instance Functor (L a) where
  fmap f x = case x of
               NilF -> NilF
               ConsF a b -> ConsF a (f b)

lengthF :: Env' a -> Int
lengthF = cata $ \x -> case x of
                        NilF -> 0
                        ConsF _ n -> n + 1

sumF :: Num a => Env' a -> a
sumF = cata $ \x -> case x of
                     NilF -> 0
                     ConsF a s -> a + s

findF :: Num a => Eq a => (a -> Bool) -> Env' a -> Maybe a
findF = \z -> cata $ \x -> case x of
                             NilF -> Nothing
                             ConsF a b -> if (z a) then Just a else b

test3 = lengthF (In (ConsF 1 (In (ConsF 2 (In NilF)))))
test4 = sumF (In (ConsF 1 (In (ConsF 2 (In NilF)))))

g x = x==2
h x = x==3

test5 = findF g (In (ConsF 1 (In (ConsF 2 (In NilF)))))
test6 = findF h (In (ConsF 1 (In (ConsF 2 (In NilF)))))

f = In (ConsF 1 (In (ConsF 2 f)))

--
-- Simple caculator with variables
--
-- Author: Perry Alexander
-- Date: Wed Jul 13 21:20:26 CDT 2016
--
-- Source files for the Binding Arithmetic Expressions extended with
-- Function (FBAE) language from PLIH
--

data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  Fix :: FBAE -> FBAE
  deriving (Show,Eq)
                    
-- Fixedpoint Operator

-- (\f. (\x . f x x) (\x . f x x))

--bind fix=lambda f in lambda x in (app (app (app f x) x) (app (app f x) x))
-- in 

-- fix :: (a -> a) -> a
-- fix f = let x = f x in x

-- fix f = (\x -> f x x)(\x -> f x x)

-- Parser

fix = parseFBAE "(bind fix = (lambda f in (lambda x in (app (app (app f x) x) (app (app f x) x)))) in 0)"
fact = parseFBAE "lambda f in if x then 0 else x + (app f (x - 1))"
total = interp "(bind fix = (lambda f in (lambda x in (app (app (app f x) x) (app (app f x) x)))) in bind fact = lambda f in if x then 0 else x + (app f (x - 1)) in (app fix fact))"



expr :: Parser FBAE
expr = buildExpressionParser operators term

operators = [ [ inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft ]
            ]

numExpr :: Parser FBAE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

identExpr :: Parser FBAE
identExpr = do i <- identifier lexer
               return (Id i)

bindExpr :: Parser FBAE
bindExpr = do reserved lexer "bind"
              i <- identifier lexer
              reservedOp lexer "="
              v <- expr
              reserved lexer "in"
              e <- expr
              return (Bind i v e)

ifExpr :: Parser FBAE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)

lambdaExpr :: Parser FBAE
lambdaExpr = do reserved lexer "lambda"
                i <- argExpr
                reserved lexer "in"
                b <- expr
                return (Lambda i b)

argExpr :: Parser String
argExpr = do i <- identifier lexer
             return i

appExpr :: Parser FBAE
appExpr = do reserved lexer "app"
             f <- expr
             a <- expr
             return (App f a)

fixExpr :: Parser FBAE
fixExpr = do reserved lexer "fix"
             t <- expr
             return (Fix t)

term = parens lexer expr
       <|> numExpr
       <|> ifExpr
       <|> bindExpr
       <|> lambdaExpr
       <|> appExpr
       <|> identExpr
       <|> fixExpr

-- Parser invocation

parseFBAE = parseString expr

parseFBAEFile = parseFile expr

-- Pretty Printer

pprint :: FBAE -> String
pprint (Num n) = show n
pprint (Id s) = s
pprint (Plus n m) = "(" ++ pprint n ++ "+" ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ "-" ++ pprint m ++ ")"
pprint (Bind n v b) = "(bind " ++ n ++ " = " ++ pprint v ++ " in " ++ pprint b ++ ")"
pprint (Lambda s b) = "(lambda " ++ s ++ " " ++ pprint b ++ ")"
pprint (App l r) = "(app " ++ pprint l ++ " " ++ pprint r ++ ")"

-- Substitution

subst :: String -> FBAE -> FBAE -> FBAE
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Bind i' v' b') = if i==i'
                            then (Bind i' (subst i v v') b')
                            else (Bind i' (subst i v v') (subst i v b'))
subst i v (Lambda i' b') = if i==i'
                           then (Lambda i' b')
                           else (Lambda i' (subst i v b'))
subst i v (App l r) = (App (subst i v l) (subst i v r))
subst i v (Id i') = if i==i'
                    then v
                    else (Id i')
subst i v (If c t e) = (If (subst i v c) (subst i v t) (subst i v e))
subst i v (Fix t) = (Fix (subst i v t))

-- Interpreter (Dynamic Scoping)

type Env = [(String,FBAE)]
         
eval :: Env -> FBAE -> FBAE
eval env (Num x) = (Num x)
eval env (Plus l r) = let (Num l') = (eval env l)
                          (Num r') = (eval env r)
                      in (Num (l'+r'))
eval env (Minus l r) = let (Num l') = (eval env l)
                           (Num r') = (eval env r)
                       in (Num (l'-r'))
eval env (Bind i v b) = let v' = eval env v in
                          eval ((i,v'):env) b
eval env (Lambda i b) = (Lambda i b)
eval env (App f a) = let (Lambda i b) = (eval env f)
                         a' = (eval env a)
                     in eval ((i,a'):env) b
eval env (Id id) = case (lookup id env) of
                     Just x -> x
                     Nothing -> error ("Varible " ++ id ++ " not found")
eval env (If c t e) = let (Num c') = (eval env c)
                      in if c'==0 then (eval env t) else (eval env e)
eval env (Fix t) = let (Lambda i b) = (eval env t) in
                     eval env (subst i (Fix (Lambda i b)) b)

interp = (eval []) .  parseFBAE


ff = (Lambda "ie" (Lambda "x" (If (Id "x") (Id "x") (Plus (Id "x") (App (Id "ie") (Minus (Id "x") (Num 1)))))))
ffs = "app (fix (lambda ie in (lambda x in if x then x else x + app ie x - 1))) 5"

ffr = (Bind "f" (Lambda "x" (If (Id "x") (Id "x") (Plus (Id "x") (App (Id "f") (Minus (Id "x") (Num 1)))))) (App (Id "f") (Num 5)))

-- Testing (Requires QuickCheck 2)

test1 = interp "(bind n = 1 in (bind f = (lambda x in x+n) in (bind n = 2 in app f 1)))"

-- Arbitrary AST Generator

instance Arbitrary FBAE where
  arbitrary =
    sized $ \n -> genFBAE ((rem n 10) + 10) []

genNum =
  do t <- choose (0,100)
     return (Num t)

genPlus n e =
  do s <- genFBAE n e
     t <- genFBAE n e
     return (Plus s t)

genMinus n e =
  do s <- genFBAE n e
     t <- genFBAE n e
     return (Minus s t)

genName =
  do i <- choose ('v','z')
     return [i]

genId e =
  do n <- elements e
     return (Id n)

genBind n e =
  do i <- genName
     v <- genFBAE n e
     b <- genFBAE n (i:e)
     return (Bind i v b)

genLambda n e =
  do i <- genName
     b <- genFBAE n (i:e)
     return (Lambda i b)

genApp n e =
  do t1 <- genFBAE n e
     t2 <- genFBAE n e
     return (App t1 t2)
     
genFBAE :: Int -> [String] -> Gen FBAE
genFBAE 0 e =
  do term <- oneof (case e of
                      [] -> [genNum]
                      _ -> [genNum
                           , (genId e)])
     return term
genFBAE n e =
  do term <- oneof [genNum
                   , (genPlus (n-1) e)
                   , (genMinus (n-1) e)
                   , (genLambda (n-1) e)
                   , (genBind (n-1) e)
                   , (genApp (n-1) e)]
     return term

-- Arbitrary AST Generator

