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

-- Calculator language extended with an environment to hold defined variables

data BAE = Num Int
         | Plus BAE BAE
         | Minus BAE BAE
         | Bind String BAE BAE
         | Id String
           deriving Show
                    
type Env = [(String,Int)]

-- R encapsulats a function from some e to some a in a constructor.  R
-- has Functor, Applicative, and Monad properties.

data R e a = R (e -> a)

instance Functor (R e) where
  fmap f (R x) = R $ \e -> (f . x) e

instance Applicative (R e) where
  pure x = R $ \e -> x
  (R f) <*> (R x) = R $ \e -> (f e) (x e)

instance Monad (R e) where
  return x = R $ \e -> x
  x >>= f = R $ \e -> runR (f (runR x e)) e

-- runR pulls the function out of the monad encapsuation and executes it.
-- Build the monad, call runR on it.
runR :: R e a -> e -> a
runR (R f) e = f e

-- ask simply returns e
ask :: R a a
ask = R $ \e -> e

-- asks applies a function to e and returns it
asks :: (e -> a) -> R e a
asks f = ask >>= \e -> (return (f e))

-- local makes local changes to e 
local :: (e -> t) -> R t a -> R e a
local f r = ask >>= \e -> return (runR r (f e))

-- lookupVar and addVar are simple utilities for looking up values in and
-- adding values to an environment.  Neither of these functions is necessary
lookupVar :: String -> Env -> Maybe Int
lookupVar = lookup

addVar :: String -> Int -> Env -> Env
addVar s i e = (s,i):e

-- evalM builds the monad for a calculation. Note that Plus and Minus
-- should be nearly identical. However, I wanted to mess with multiple
-- implementations, thus you'll see to ways of executing binary operations.
evalM (Num n) = return n
evalM (Plus l r) = liftM2 (+) (evalM l) (evalM r)
evalM (Minus l r) = do
  l' <- (evalM l)
  r' <- (evalM r)
  return (l' - r')
evalM (Id id) = do
  env <- ask
  return (case (lookupVar id env) of
            Just x -> x
            Nothing -> error "Variable not found")
evalM (Bind i v b) = do
  env <- ask
  v' <- evalM v
  local (addVar i v') (evalM b)
