{-# LANGUAGE GADTs #-}

-- Imports for QuickCheck
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic

-- Imports for Parsec
import Control.Monad
import Control.Applicative
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

-- Imports for PLIH
import ParserUtils

-- Untyped arithmetic interpreter extended with untyped functions using the
-- Reader monad
--
-- Author: Perry Alexander
-- Date: Wed Jul 13 21:20:26 CDT 2016
--
-- Source files for the Functions Binding Arithmetic Expressions extended
-- with Function (FBAE) language from PLIH
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
                    
type Env = [(String,FBAE)]

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

-- addVar is a simple utility for adding values to an environment.

addVar :: String -> FBAE -> Env -> Env
addVar s i e = (s,i):e

-- evalR builds the monad for a calculation. Note that Plus and Minus
-- should be nearly identical. However, I wanted to mess with multiple
-- implementations, thus you'll see to ways of executing binary operations.
evalR :: FBAE -> Reader Env FBAE
evalR (Num n) = return (Num n)
evalR (Plus l r) = do { 
  (Num l') <- (evalR l);
  (Num r') <- (evalR r);
  return (Num (l'+r')) }
evalR (Minus l r) = do {
  (Num l') <- (evalR l) ;
  (Num r') <- (evalR r) ;
  return (Num (l'-r')) }
evalR (Id id) = do {
  env <- ask ;
  return (case (lookup id env) of
             Just x -> x
             Nothing -> error "Variable not found") }
evalR (Bind i v b) = do {
  v' <- evalR v ;
  local (addVar i v') (evalR b) }
evalR (Lambda i b) = return (Lambda i b)
evalR (App f v) = do {
  (Lambda i b) <- evalR f ;
  v' <- evalR v ;
  local (addVar i v') (evalR b) }

eval x = runR (evalR x) []

data FBAETy where
  TNum :: FBAETy
  TFun :: FBAETy -> FBAETy -> FBAETy
  deriving (Show,Eq)

type Cont = [(String,FBAETy)]

-- addVarTy is a the same as addVar except it adds a type to the context
-- instead of an expression to the environment.

addVarTy :: String -> FBAETy -> Cont -> Cont
addVarTy s i e = (s,i):e

typeofR :: FBAE -> Reader Cont FBAETy
typeofR (Num n) = return TNum
typeofR (Plus l r) = do {
  TNum <- (typeofR l) ;
  TNum <- (typeofR r) ;
  return TNum }
typeofR (Minus l r) = do {
  TNum <- (typeofR l) ;
  TNum <- (typeofR r) ;
  return TNum }
typeofR (Id id) = do {
  env <- ask ;
  return (case (lookup id env) of
             Just x -> x
             Nothing -> error "Variable not found") }
typeofR (Bind i v b) = do {
  con <- ask ;
  v' <- typeofR v ;
  local (addVarTy i v') (typeofR b) }
typeofR (Lambda i b) = do {
  r' <- local (addVarTy i TNum) (typeofR b) ;
  return (TFun TNum r') }
typeofR (App f v) = do {
  (TFun i b) <- typeofR f ;
  v' <- typeofR v ;
  return (if i==v' then b else error "Type Error in app") }

typeof x = runR (typeofR x) []

interp x = let ty = typeof x in eval x

test1 = interp (Num 1)
test2 = interp (App (Lambda "x" (Plus (Id "x") (Num 1))) (Num 1))



