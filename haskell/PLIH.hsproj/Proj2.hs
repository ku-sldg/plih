{-# LANGUAGE GADTs #-}

module Proj2 where

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
-- Simple caculator with variables extended Booleans and both static and
-- dynamic type checking.
--
-- Author: Perry Alexander
-- Date: Wed Jul 13 11:24:46 CDT 2016
--
-- Source files for the Boolean Binding Arithmetic Expressions (BBAE)
-- language from PLIH
--

data TBBAE where
  TNum :: TBBAE
  TBool :: TBBAE
  deriving (Show,Eq)

data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: (String,BBAE) -> BBAE -> BBAE
  Binds :: [(String,BBAE)] -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  Seq :: BBAE -> BBAE -> BBAE
  Print :: BBAE -> BBAE
  Cons :: BBAE -> BBAE -> BBAE
  First :: BBAE -> BBAE
  Rest :: BBAE -> BBAE
  IsEmpty :: BBAE -> BBAE
  Empty :: BBAE
  deriving (Show,Eq)

-- AST Pretty Printer

pprint :: BBAE -> String
pprint (Num n) = show n
pprint (Boolean b) = show b
pprint (Plus n m) = "(" ++ pprint n ++ " + " ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ " - " ++ pprint m ++ ")"
pprint (And n m) = "(" ++ pprint n ++ " && " ++ pprint m ++ ")"
pprint (Leq n m) = "(" ++ pprint n ++ " <= " ++ pprint m ++ ")"
pprint (IsZero m) = "(isZero " ++ pprint m ++ ")"
pprint (If c n m) = "(if " ++ pprint c ++ " then " ++ pprint n ++ " else " ++ pprint m ++ ")"
pprint (Id s) = s
pprint (Bind (n,v) b) = "(bind " ++ n ++ " = " ++ pprint v ++ " in " ++ pprint b ++ ")"

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
              return (Bind (i,v) e)

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
            
seqExpr :: Parser BBAE
seqExpr = do reserved lexer "seq"
             f <- expr
             s <- expr
             return (Seq f s)

printExpr :: Parser BBAE
printExpr = do reserved lexer "print"
               t <- expr
               return (Print t)

consExpr :: Parser BBAE
consExpr = do reserved lexer "cons"
              f <- expr
              s <- expr
              return (Cons f s)

firstExpr :: Parser BBAE
firstExpr = do reserved lexer "first"
               t <- expr
               return (First t)
             
restExpr :: Parser BBAE
restExpr = do reserved lexer "rest"
              t <- expr
              return (Rest t)

isEmptyExpr :: Parser BBAE
isEmptyExpr = do reserved lexer "isEmpty"
                 t <- expr
                 return (IsEmpty t)

emptyExpr :: Parser BBAE
emptyExpr = do reserved lexer "empty"
               return Empty
             
term = parens lexer expr
       <|> numExpr
       <|> identExpr
       <|> bindExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr
       <|> consExpr
       <|> firstExpr
       <|> restExpr              
       <|> isEmptyExpr
       <|> emptyExpr
       <|> printExpr
       <|> seqExpr
       
parseBAE = parseString expr

parseBAEFile = parseFile expr

-- Parser invocation

parseBBAE = parseString expr

parseBBAEFile = parseFile expr

-- Substitution

subst :: String -> BBAE -> BBAE -> BBAE
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Bind (i',v') b') = if i==i'
                              then (Bind (i',(subst i v v')) b')
                              else (Bind (i',(subst i v v')) (subst i v b'))
subst i v (Binds l b) = Binds (map (\(i',v') -> (i',(subst i v v'))) l) (subst i v b)
subst i v (Id i') = if i==i'
                    then v
                    else (Id i')
subst i v (Boolean b) = (Boolean b)
subst i v (And l r) = (And (subst i v l) (subst i v r))
subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))
subst i v (IsZero t) = (IsZero (subst i v t))
subst i v (If c t e) = (If (subst i v c) (subst i v t) (subst i v e))
       
liftNum :: (Int -> Int -> Int) -> BBAE -> BBAE -> BBAE
liftNum f l r = let (Num l') = l
                    (Num r') = r
                in (Num (f l' r'))

liftBoolean :: (Bool -> Bool -> Bool) -> BBAE -> BBAE -> BBAE
liftBoolean f l r = let (Boolean l') = l
                        (Boolean r') = r
                    in (Boolean (f l' r'))

evals :: BBAE -> (Either String BBAE)
evals (Num x) = (return (Num x))
evals (Plus l r) = (liftM2 (liftNum (+)) (evals l) (evals r))
evals (Minus l r) = (liftM2 (liftNum (+)) (evals l) (evals r))
evals (Bind (i,v) b) = do { v' <- evals v ;
                            (evals (subst i v' b)) }
evals (Binds [] b') = evals b'
evals (Binds ((i,v):bls) b) = do { v' <- evals v ;
                                   evals (Binds bls (subst i v' b)) }
evals (Id id) = (Left "Undeclared Variable")
evals (Boolean b) = return (Boolean b)
evals (And l r) = (liftM2 (liftBoolean (&&)) (evals l) (evals r))
evals (Leq l r) = do { (Num l') <- (evals l) ;
                       (Num r') <- (evals r) ;
                       return (Boolean (l' <= r')) }
evals (IsZero t) = do { t' <- (evals t) ;
                        (return (Boolean (t' == (Num 0)))) ;
                        }
evals (If c t e) = do { (Boolean c') <- (evals c) ;
                        if c' then (evals t) else (evals e) ;
                        }
evals (Seq f s) = do { (evals f) ;
                       (evals s) }
evals (Print t) = do { t' <- (evals t) ;
                       let r = (print t') in (return (Num 0)) }

interps = evals . parseBAE


-- Defered Substitution

type Env = [(String,BBAE)]
type Cont = [(String,TBBAE)]
    
eval ::  Env -> BBAE -> (Either String BBAE)
eval env (Num x) = return (Num x)
eval env (Plus l r) = do { (Num l') <- (eval env l) ;
                           (Num r') <- (eval env r) ;
                           return (Num (l'+r'))}
eval env (Minus l r) = do { (Num l') <- (eval env l) ;
                            (Num r') <- (eval env r) ;
                            return (Num (l'-r'))}
eval env (Bind (i,v) b) = do { v' <- eval env v ;
                               eval ((i,v'):env) b }
eval env (Binds [] b) = eval env b
eval env (Binds ((i,v):bls) b) = do { v' <- eval env v ;
                                      eval ((i,v'):env) (Binds bls b) }
eval env (Id id) = case (lookup id env) of
                     Just x -> return x
                     Nothing -> (Left "Varible not found")
eval env (Boolean b) = return (Boolean b)
eval env (And l r) = do { (Boolean l') <- (eval env l) ;
                          (Boolean r') <- (eval env r) ;
                          return (Boolean (l' && r')) }
eval env (Leq l r) = do { (Num l') <- (eval env l) ;
                          (Num r') <- (eval env r) ;
                          return (Boolean (l' <= r')) }
eval env (IsZero v) = do { (Num v') <- (eval env v) ;
                           return (Boolean (v' == 0)) }
eval env (If c t e) = do { (Boolean c') <- (eval env c) ;
                           if c' then (eval env t) else (eval env e) }

interp = (eval []) . parseBBAE


typeof :: Cont -> BBAE -> Either String TBBAE
typeof cont (Num x) = (Right TNum)
typeof cont (Plus l r) = let l' = (typeof cont l)
                             r' = (typeof cont r)
                         in if l'==(Right TNum) && r'==(Right TNum)
                            then (Right TNum)
                            else (Left "Type Mismatch in +")
typeof cont (Minus l r) = let l' = (typeof cont l)
                              r' = (typeof cont r)
                          in if l'==(Right TNum) && r'==(Right TNum)
                             then (Right TNum)
                             else (Left "Type Mismatch in -")
typeof cont (Bind (i,v) b) = let v' = typeof cont v in
                             case v' of
                               (Right v'') -> typeof ((i,v''):cont) b
                               (Left _) -> v'
typeof cont (Binds [] b) = typeof cont b
typeof cont (Binds ((i,v):bls) b) = do { v' <- typeof cont v ;
                                         typeof ((i,v'):cont) (Binds bls b) }
typeof cont (Id id) = case (lookup id cont) of
                        Just x -> (Right x)
                        Nothing -> (Left "Varible not found")
typeof cont (Boolean b) = (Right TBool)
typeof cont (And l r) = if (typeof cont l) == (Right TBool)
                           && (typeof cont r) == (Right TBool)
                        then (Right TBool)
                        else (Left "Type mismatch in &&")
typeof cont (Leq l r) = if (typeof cont l) == (Right TNum)
                           && (typeof cont r) == (Right TNum)
                        then (Right TBool)
                        else (Left "Type mismatch in <=")
typeof cont (IsZero v) = if (typeof cont v) == (Right TNum)
                         then (Right TBool)
                         else (Left "Type mismatch in IsZero")
typeof cont (If c t e) = if (typeof cont c) == (Right TBool)
                            && (typeof cont t)==(typeof cont e)
                         then (typeof cont e)
                         else (Left "Type mismatch in if")

interpTyped :: String -> Either String BBAE
interpTyped e = let p=(parseBBAE e) in
                  case (typeof [] p) of
                    (Right _) -> (eval [] p)
                    (Left m) -> (Left m)

-- Simple optimizer that removes zero terms

optimize :: BBAE -> BBAE
optimize (Num x) = (Num x)
optimize (Plus (Num 0) r) = optimize r
optimize (Plus l (Num 0)) = optimize l
optimize (Plus l r) = let l' = (optimize l)
                          r' = (optimize r)
                      in (Plus l' r')
optimize (Minus l (Num 0)) = optimize l
optimize (Minus l r) = let l' = (optimize l)
                           r' = (optimize r)
                       in (Minus l' r')
optimize (Bind (i,v) b) = let v' = optimize v in
                          optimize b
optimize (Id id) = (Id id)
optimize (Boolean b) = (Boolean b)
optimize (And l r) = (And  (optimize l) (optimize r))
optimize (Leq l r) = (Leq (optimize l) (optimize r))
optimize (IsZero v) = (IsZero (optimize v))
optimize (If c t e) = (If (optimize c) (optimize t) (optimize e))


-- Arbitrary AST Generator

instance Arbitrary BBAE where
  arbitrary =
    sized $ \n -> genBBAE (rem n 10) []

genNum =
  do t <- choose (0,100)
     return (Num t)

genId e =
  do n <- elements e
     return (Id n)

genPlus n e =
  do s <- genBBAE n e
     t <- genBBAE n e
     return (Plus s t)

genMinus n e =
  do s <- genBBAE n e
     t <- genBBAE n e
     return (Minus s t)

genAnd n e =
  do s <- genBBAE n e
     t <- genBBAE n e
     return (And s t)

genLeq n e =
  do s <- genBBAE n e
     t <- genBBAE n e
     return (Leq s t)

genIsZero n e =
  do s <- genBBAE n e
     return (IsZero s)

genIf n e =
  do s <- genBBAE n e
     t <- genBBAE n e
     u <- genBBAE n e
     return (If s t u)

genBind n e =
  do i <- genName
     v <- genBBAE n e
     b <- genBBAE n (i:e)
     return (Bind (i,v) b)
     
genBinds n e =
  do i1 <- genName
     v1 <- genBBAE n e
     i2 <- genName
     v2 <- genBBAE n e
     b <- genBBAE n (i1:(i2:e))
     return (Binds [(i1,v1),(i2,v2)] b)

genName =
  do i <- choose ('v','z')
     return [i]

genBool =
  do t <- choose (True,False)
     return (Boolean t)

genBBAE :: Int -> [String] -> Gen BBAE
genBBAE 0 e = 
  do term <- oneof (case e of
                      [] -> [genNum,genBool]
                      _ -> [genNum
                           , genBool
                           , (genId e)])
     return term
genBBAE n e =
  do term <- oneof [genNum
                   , (genPlus (n-1) e)
                   , (genMinus (n-1) e)
                   , (genAnd (n-1) e)
                   , (genLeq (n-1) e)
                   , (genIsZero (n-1) e)
                   , (genIf (n-1) e)
                   , (genBinds (n-1) e)
                   ]
     return term

-- QuickCheck 

testParser :: Int -> IO ()
testParser n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> parseBBAE (pprint t) == t)

testEval :: Int -> IO ()
testEval n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interp $ pprint t) == (eval [] t))

testTypeof :: Int -> IO ()
testTypeof n = quickCheckWith stdArgs {maxSuccess=n}
  (\t-> case (typeof [] t) of
      (Right _) -> True
      (Left _) -> True)

testTypedEval :: Int -> IO ()
testTypedEval n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> case typeof [] t of
           (Right _) -> ((eval []) . parseBBAE . pprint) t == (eval [] t)
           (Left _) -> True)

testOptimizedEval :: Int -> IO ()
testOptimizedEval n =
  quickCheckWith stdArgs {maxSuccess=n}
  (\t -> case typeof [] t of
           (Right _) -> (eval [] . optimize) t == (eval [] t)
           (Left _) -> True)
