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
-- Arithmetic expression langage extended with bind, functions and static
-- scoping.
--
-- Author: Perry Alexander
-- Date: Wed Jul 13 21:20:26 CDT 2016
--
-- Source files for the Binding Arithmetic Expressions extended with
-- Functions and Static Scoping (FBAES) language from PLIH
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
  deriving (Show,Eq)
                    
-- Parser

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

term = parens lexer expr
       <|> numExpr
       <|> ifExpr
       <|> bindExpr
       <|> lambdaExpr
       <|> appExpr
       <|> identExpr

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
       
evalS :: FBAE -> FBAE
evalS (Num x) = (Num x)
evalS (Plus l r) = let (Num l') = (evalS l)
                       (Num r') = (evalS r)
                   in (Num (l' + r'))
evalS (Minus l r) = let (Num l') = (evalS l)
                        (Num r') = (evalS r)
                    in (Num (l' - r'))
evalS (Bind i v b) = (evalS (subst i (evalS v) b))
evalS (Lambda i b) = (Lambda i b)
evalS (App f a) = let (Lambda i b) = (evalS f)
                      a' = (evalS a)
                  in evalS (subst i (evalS a) b)
evalS (If c t e) = let (Num c') = (evalS c)
                   in if c'==0 then (evalS t) else (evalS e)
evalS (Id id) = error "Undeclared Variable"

interpS = evalS . parseFBAE

-- Interpreter (Static Scoping)

data FBAEVal where
  NumV :: Int -> FBAEVal
  ClosureV :: String -> FBAE -> Env -> FBAEVal
  deriving (Show,Eq)

type Env = [(String,FBAEVal)]
         
evalM :: Env -> FBAE -> Maybe FBAEVal
evalM env (Num x) = return (NumV x)
evalM env (Plus l r) = do { (NumV l') <- (evalM env l);
                           (NumV r') <- (evalM env r);
                           return (NumV (l'+r'))}
evalM env (Minus l r) = do { (NumV l') <- (evalM env l);
                            (NumV r') <- (evalM env r);
                            return (NumV (l'-r'))}
evalM env (Bind i v b) = do { v' <- evalM env v;
                             evalM ((i,v'):env) b }
evalM env (Lambda i b) = return (ClosureV i b env)
evalM env (App f a) = do { (ClosureV i b e) <- (evalM env f);
                          a' <- (evalM env a); 
                          evalM ((i,a'):e) b }
evalM env (Id id) = lookup id env
evalM env (If c t e) = do { (NumV c') <- (evalM env c);
                           if c'==0 then (evalM env t) else (evalM env e) }


interp = (evalM []) .  parseFBAE


-- Testing (Requires QuickCheck 2)

test1 = interp "(bind n = 1 in (bind f = (lambda x in x+n) in (bind n = 2 in app f 1)))"

test2 = let expr = "(bind n = 1 in (bind f = (lambda x in x+n) in (bind n = 2 in app f 1)))"
        in let (Num v2) = interpS expr
           in do { (NumV v1) <- interp expr ;
                   return (v1 == v2) }

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


-- Combinators

-- Omega - Infinite combinator

omega = (App (Lambda "x" (App (Id "x") (Id "x"))) (Lambda "x" (App (Id "x") (Id "x"))))

-- Y - Fixed point Y combinator

y = (Lambda "f" (App (Lambda "x" (App (Id "f") (App (Id "x") (Id "x"))))
                     (Lambda "x" (App (Id "f") (App (Id "x") (Id "x"))))))

-- Z - Applicative Y combinator

z = (Lambda "f" (App (Lambda "x" (App (Id "f") (Lambda "v" (App (App (Id "x") (Id "x")) (Id "v")))))
                     (Lambda "x" (App (Id "f") (Lambda "v" (App (App (Id "x") (Id "x")) (Id "v")))))))


-- Test Function

ff = (Lambda "ie" (Lambda "x" (If (Id "x") (Id "x") (Plus (Id "x") (App (Id "ie") (Minus (Id "x") (Num 1)))))))
