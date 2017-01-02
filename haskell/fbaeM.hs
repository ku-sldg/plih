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
-- has Functor, Applicative, and Monad properties.

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

numPlus :: FBAE -> FBAE -> FBAE
numPlus (Num l) (Num r) = Num (l + r)
numPlus _ _ = error "Non-Num argument to numop"

numMinus :: FBAE -> FBAE -> FBAE
numMinus (Num l) (Num r) = Num (l + r)
numMinus _ _ = error "Non-Num argument to numop"

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
  env <- ask
  return (case (lookupVar id env) of
            Just x -> x
            Nothing -> error "Variable not found")
evalM (Bind i v b) = do
  env <- ask
  v' <- evalM v
  local (addVar i v') (evalM b)
evalM (Lambda i b) = return (Lambda i b)
evalM (App f v) = do
  env <- ask
  (Lambda i b) <- evalM f
  v' <- evalM v
  local (addVar i v') (evalM b)

interp x = runR (evalM x) []

test1 = interp (Num 1)
test2 = interp (App (Lambda "x" (Plus (Id "x") (Num 1))) (Num 1))



