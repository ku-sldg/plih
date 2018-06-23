{-# LANGUAGE GADTs #-}

import Text.ParserCombinators.Parsec
import Control.Monad
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Calculator language extended with an environment to hold defined variables

data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  deriving (Show,Eq)

data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Mult :: FBAE -> FBAE -> FBAE
  Div :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> TFBAE -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  Boolean :: Bool -> FBAE
  And :: FBAE -> FBAE -> FBAE
  Or :: FBAE -> FBAE -> FBAE
  Leq :: FBAE -> FBAE -> FBAE
  IsZero :: FBAE -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  Fix :: FBAE -> FBAE
  deriving (Show,Eq)

type Env = [(String,FBAEVal)]
type Cont = [(String,TFBAE)]

data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> TFBAE -> FBAE -> Env -> FBAEVal
  deriving (Show,Eq)

-- Substitution

subst :: String -> FBAE -> FBAE -> FBAE
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))
subst i v (Div l r) = (Div (subst i v l) (subst i v r))
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

subst i v (Boolean b) = (Boolean b)
subst i v (And l r) = let l' = (subst i v l)
                          r' = (subst i v r)
                      in (And l' r')
subst i v (Or l r) = let l' = (subst i v l)
                         r' = (subst i v r)
                     in (Or l' r')
subst i v (Leq l r) = let l' = (subst i v l)
                          r' = (subst i v r)
                      in (Leq l' r')
subst i v (IsZero t) = let t' = (subst i v t)
                       in (IsZero t')

subst i v (If c t e) = (If (subst i v c) (subst i v t) (subst i v e))
subst i v (Fix t) = (Fix (subst i v t))

evalM :: Env -> FBAE -> (Maybe FBAEVal)
evalM env (Num x) = return (NumV x)
evalM env (Plus l r) = do { (NumV l') <- (evalM env l) ;
                           (NumV r') <- (evalM env r) ;
                           return (NumV (l'+r')) }
evalM env (Minus l r) = do { (NumV l') <- (evalM env l) ;
                            (NumV r') <- (evalM env r) ;
                            return (NumV (l'-r')) }
evalM env (Mult l r) = do { (NumV l') <- (evalM env l) ;
                           (NumV r') <- (evalM env r) ;
                           return (NumV (l'*r')) }
evalM env (Div l r) = do { (NumV l') <- (evalM env l) ;
                          (NumV r') <- (evalM env r) ;
                          return (NumV (div l' r')) }
evalM env (Bind i v b) = do { v' <- evalM env v ;
                             evalM ((i,v'):env) b }
evalM env (Lambda i t b) = return (ClosureV i t b env)
evalM env (App f a) = do { (ClosureV i t b e) <- (evalM env f) ;
                          a' <- (evalM env a) ;
                          (evalM ((i,a'):e) b) }
evalM env (Id id) = (lookup id env)
evalM env (Boolean b) = return (BooleanV b)
evalM env (And l r) = do { (BooleanV l') <- (evalM env l) ;
                          (BooleanV r') <- (evalM env r) ;
                          return (BooleanV (l' && r')) }
evalM env (Or l r) = do { (BooleanV l') <- (evalM env l) ;
                         (BooleanV r') <- (evalM env r) ;
                         return (BooleanV (l' || r')) }
evalM env (Leq l r) = do { (NumV l') <- (evalM env l) ;
                          (NumV r') <- (evalM env r) ;
                          return (BooleanV (l' <= r')) }
evalM env (IsZero v) = do { (NumV v') <- (evalM env v) ;
                           return (BooleanV (v' == 0)) }
evalM env (If c t e) = do { (BooleanV c') <- (evalM env c) ;
                           (if c' then (evalM env t) else (evalM env e)) }
evalM env (Fix f) = do { (ClosureV i ty b e) <- (evalM env f) ;
                         evalM e (subst i (Fix (Lambda i ty b)) b) }


typeofM :: Cont -> FBAE -> (Maybe TFBAE)
typeofM cont (Num x) = return TNum
typeofM cont (Plus l r) = do { TNum <- (typeofM cont l) ;
                               TNum <- (typeofM cont r) ;
                               return TNum}
typeofM cont (Minus l r) = do { TNum <- (typeofM cont l) ;
                                TNum <- (typeofM cont r) ;
                                return TNum }
typeofM cont (Mult l r) = do { TNum <- (typeofM cont l) ;
                               TNum <- (typeofM cont r) ;
                               return TNum }
typeofM cont (Div l r) = do { TNum <- (typeofM cont l) ;
                              TNum <- (typeofM cont r) ;
                              return TNum}
typeofM cont (Bind i v b) = do { v' <- typeofM cont v ;
                                 typeofM ((i,v'):cont) b }
typeofM cont (Id id) = (lookup id cont)
typeofM cont (Lambda x t b) = do { tyB <- typeofM ((x,t):cont) b ;
                                   return (t :->: tyB) }
typeofM cont (App x y) = do { tyXd :->: tyXr <- typeofM cont x ;
                              tyY <- typeofM cont y ;
                              if tyXd==tyY
                              then return tyXr
                              else Nothing }
typeofM cont (Boolean b) = return TBool
typeofM cont (And l r) = do { TBool <- (typeofM cont l) ;
                              TBool <- (typeofM cont r) ;
                              return TBool }
typeofM cont (Or l r) = do { TBool <- (typeofM cont l) ;
                             TBool <- (typeofM cont r) ;
                             return TBool}
typeofM cont (Leq l r) = do { TNum <- (typeofM cont l) ;
                              TNum <- (typeofM cont r) ;
                              return TBool }
typeofM cont (IsZero v) = do { TNum <- (typeofM cont v) ;
                               return TBool }
typeofM cont (If c t e) = do { TBool <- (typeofM cont c) ;
                               t' <- (typeofM cont t) ;
                               e' <- (typeofM cont e) ;
                               if t'==e'
                               then return t'
                               else Nothing }
typeofM cont (Fix f) = do { ((:->:) d r) <- (typeofM cont f) ;
                            return r }

test1 = (Bind "f" (Lambda "g" ((:->:) TNum TNum)
                    (Lambda "x" TNum (If (IsZero (Id "x")) (Num 1)
                                         (Mult (Id "x")
                                               (App (Id "g")
                                                    (Minus (Id "x")
                                                           (Num 1)))))))
         (App (Fix (Id "f")) (Num 3)))

