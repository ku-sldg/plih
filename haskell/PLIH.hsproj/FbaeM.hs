{-# LANGUAGE GADTs #-}

module FbaeM where

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

-- lookupVar and addVar are simple utilities for looking up values in and
-- adding values to an environment.  Neither of these functions is necessary
lookupVar :: String -> Env -> Maybe FBAE
lookupVar = lookup

addVar :: String -> FBAE -> Env -> Env
addVar s i e = (s,i):e

liftNum :: (Int -> Int -> Int) -> FBAE -> FBAE -> FBAE
liftNum f (Num t1) (Num t2) = (Num (f t1 t2))

numPlus = liftNum (+)
numMinus = liftNum (-)

-- evalM builds the monad for a calculation. Note that Plus and Minus
-- should be nearly identical. However, I wanted to mess with multiple
-- implementations, thus you'll see to ways of executing binary operations.
evalM :: FBAE -> Reader Env FBAE
evalM (Num n) = return (Num n)
evalM (Plus l r) = do
  l' <- (evalM l)
  r' <- (evalM r)
  return (numPlus l' r')
evalM (Minus l r) = do
  l' <- (evalM l)
  r' <- (evalM r)
  return (numMinus l' r')
evalM (Id id) = do
  ask >>= \env -> return (case (lookupVar id env) of
                             Just x -> x
                             Nothing -> error "Variable not found")
evalM (Bind i v b) = do
  v' <- evalM v
  local (addVar i v') (evalM b)
evalM (Lambda i b) = return (Lambda i b)
evalM (App f v) = do
  (Lambda i b) <- evalM f
  v' <- evalM v
  local (addVar i v') (evalM b)

eval x = runR (evalM x) []

data FBAETy where
  TNum :: FBAETy
  TFun :: FBAETy -> FBAETy -> FBAETy
  deriving (Show,Eq)

type Cont = [(String,FBAETy)]

lookupVarTy = lookup
addVarTy :: String -> FBAETy -> Cont -> Cont
addVarTy s i e = (s,i):e

typeofM :: FBAE -> Reader Cont FBAETy
typeofM (Num n) = return TNum
typeofM (Plus l r) = do
  l' <- (typeofM l)
  r' <- (typeofM r)
  return (if (l'==TNum && r'==TNum) then TNum else error "Type error in +")
typeofM (Minus l r) = do
  l' <- (typeofM l)
  r' <- (typeofM r)
  return (if (l'==TNum && r'==TNum) then TNum else error "Type error in -")
typeofM (Id id) = do
  ask >>= \env -> return (case (lookupVarTy id env) of
                            Just x -> x
                            Nothing -> error "Variable not found")
typeofM (Bind i v b) = do
  con <- ask
  v' <- typeofM v
  local (addVarTy i v') (typeofM b)
typeofM (Lambda i b) = do
  r' <- local (addVarTy i TNum) (typeofM b)
  return (TFun TNum r')
typeofM (App f v) = do
  (TFun i b) <- typeofM f
  v' <- typeofM v
  return (if i==v' then b else error "Type Error in app")

typeof x = runR (typeofM x) []

interp x = let ty = typeof x in eval x

test1 = interp (Num 1)
test2 = interp (App (Lambda "x" (Plus (Id "x") (Num 1))) (Num 1))



