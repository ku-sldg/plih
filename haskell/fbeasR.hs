{-# LANGUAGE GADTs #-}

-- Imports for QuickCheck
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic

-- Imports for Parser
import Control.Monad
import Control.Applicative
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

-- Imports for PLIH
import ParserUtils

--
-- Arithmetic expression langage extended with bind, functions and static
-- scoping using Reader monad
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
  Lambda :: String -> FBAETy -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  deriving (Show,Eq)

-- Reader encapsulats a function from some e to some a in a constructor.  Reader
-- is an instance of Functor, Applicative, and Monad. 

data Reader e a = Reader (e -> a)

instance Functor (Reader e) where
  fmap f (Reader g) = Reader $ \e -> (f . g) e

instance Applicative (Reader e) where
  pure x = Reader $ \e -> x
  (Reader f) <*> (Reader g) = Reader $ \e -> (f e) (g e)

instance Monad (Reader e) where
  return x = Reader $ \e -> x
  g >>= f = Reader $ \e -> runR (f (runR g e)) e

-- runR pulls the function out of the monad encapsuation and executes it.
-- Build the monad, call runR on it.
runR :: Reader e a -> e -> a
runR (Reader f) e = f e

-- ask simply returns e
ask :: Reader a a
ask = Reader $ \e -> e

-- asks applies a function to e and returns it
asks :: (e -> a) -> Reader e a
asks f = ask >>= \e -> (return (f e))

-- local makes local changes to e 
local :: (e -> t) -> Reader t a -> Reader e a
local f r = ask >>= \e -> return (runR r (f e))

explicit :: e -> Reader e a -> Reader e a
explicit e r = return (runR r e)

-- lookupVar and addVar are simple utilities for looking up values in and
-- adding values to an environment.  Neither of these functions is necessary
lookupVar :: String -> Env -> Maybe FBAEVal
lookupVar = lookup

addVar :: String -> FBAEVal -> Env -> Env
addVar s i e = (s,i):e

useClosure :: String -> FBAEVal -> Env -> Env -> Env
useClosure s i e _ = (s,i):e

liftNum :: (Int -> Int -> Int) -> FBAE -> FBAE -> FBAE
liftNum f (Num t1) (Num t2) = (Num (f t1 t2))

numPlus = liftNum (+)
numMinus = liftNum (-)

-- Substitution

subst :: String -> FBAE -> FBAE -> FBAE
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Bind i' v' b') = if i==i'
                            then (Bind i' (subst i v v') b')
                            else (Bind i' (subst i v v') (subst i v b'))
subst i v (Lambda i' t' b') = if i==i'
                           then (Lambda i' t' b')
                           else (Lambda i' t' (subst i v b'))
subst i v (App l r) = (App (subst i v l) (subst i v r))
subst i v (Id i') = if i==i'
                    then v
                    else (Id i')
       
evals :: FBAE -> FBAE
evals (Num x) = (Num x)
evals (Plus l r) = let (Num l') = (evals l)
                       (Num r') = (evals r)
                   in (Num (l' + r'))
evals (Minus l r) = let (Num l') = (evals l)
                        (Num r') = (evals r)
                    in (Num (l' - r'))
evals (Bind i v b) = (evals (subst i (evals v) b))
evals (Lambda i t b) = (Lambda i t b)
evals (App f a) = let (Lambda i t b) = (evals f)
                      a' = (evals a)
                  in evals (subst i (evals a) b)
evals (If c t e) = let (Num c') = (evals c)
                   in if c'==0 then (evals t) else (evals e)
evals (Id id) = error "Undeclared Variable"

interps = evals

-- Interpreter (Static Scoping)

data FBAEVal where
  NumV :: Int -> FBAEVal
  ClosureV :: String -> FBAE -> Env -> FBAEVal
  deriving (Show,Eq)

type Env = [(String,FBAEVal)]
         
evalR :: FBAE -> Reader Env FBAEVal
evalR (Num x) = return (NumV x)
evalR (Plus l r) = do
  (NumV l') <- (evalR l)
  (NumV r') <- (evalR r)
  return (NumV (l'+r'))
evalR (Minus l r) = do
  (NumV l') <- (evalR l)
  (NumV r') <- (evalR r)
  return (NumV (l'-r'))
evalR (Bind i v b) = do
  v' <- evalR v
  local (addVar i v') (evalR b)
evalR (Lambda i _ b) = do
  env <- ask
  return (ClosureV i b env)
evalR (App f a) = do
  (ClosureV i b e) <- (evalR f)
  a' <- (evalR a)
  explicit ((i,a'):e) (evalR b)
evalR (Id id) = do
  env <- ask
  case (lookup id env) of
    Just x -> return x
    Nothing -> error "Varible not found"
evalR (If c t e) = do
  (NumV c') <- (evalR c)
  if c'==0 then (evalR t) else (evalR e)

interp x = runR (evalR x) []

-- Typeof

data FBAETy where
  TNum :: FBAETy
  TFun :: FBAETy -> FBAETy -> FBAETy
  deriving (Show,Eq)

type Cont = [(String,FBAETy)]

lookupVarTy = lookup
addVarTy :: String -> FBAETy -> Cont -> Cont
addVarTy s i e = (s,i):e

typeofR :: FBAE -> Reader Cont FBAETy
typeofR (Num n) = return TNum
typeofR (Plus l r) = do
  l' <- (typeofR l)
  r' <- (typeofR r)
  return (if (l'==TNum && r'==TNum) then TNum else error "Type error in +")
typeofR (Minus l r) = do
  l' <- (typeofR l)
  r' <- (typeofR r)
  return (if (l'==TNum && r'==TNum) then TNum else error "Type error in -")
typeofR (Id id) = do
  ask >>= \env -> return (case (lookupVarTy id env) of
                            Just x -> x
                            Nothing -> error "Variable not found")
typeofR (Bind i v b) = do
  con <- ask
  v' <- typeofR v
  local (addVarTy i v') (typeofR b)
typeofR (Lambda i t b) = do
  r' <- local (addVarTy i t) (typeofR b)
  return (TFun t r')
typeofR (App f v) = do
  (TFun i b) <- typeofR f
  v' <- typeofR v
  return (if i==v' then b else error "Type Error in app")

typeof x = runR (typeofR x) []


-- Testing (Requires QuickCheck 2)

testExpr = (Bind "n" (Num 1)
             (Bind "f" (Lambda "x" TNum (Plus (Id "x") (Id "n")))
               (Bind "n" (Num 2) (App (Id "f") (Num 1)))))

test1 = interp (Bind "n" (Num 1)
                (Bind "f" (Lambda "x" TNum (Plus (Id "x") (Id "n")))
                 (Bind "n" (Num 2) (App (Id "f") (Num 1)))))

test2 = let expr = (Bind "n" (Num 1)
                (Bind "f" (Lambda "x" TNum (Plus (Id "x") (Id "n")))
                 (Bind "n" (Num 2) (App (Id "f") (Num 1)))))
        in let (NumV v1) = interp expr
           in let (Num v2) = interps expr
              in v1 == v2

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

-- This is wrong.  Need to generate a type here.
genLambda n e =
  do i <- genName
     b <- genFBAE n (i:e)
     return (Lambda i TNum b)

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

