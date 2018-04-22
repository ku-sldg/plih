{-# LANGUAGE GADTs #-}

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

-- Imports for PLIH
import ParserUtils

--
-- Untyped arithmetic interpreter extended with bind and untyped functions
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

-- appExpr :: Parser FBAE
-- appExpr = do reserved lexer "app"
--              f <- expr
--              a <- expr
--              return (App f a)

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
       <|> identExpr
       <|> appExpr

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
       
evalS :: FBAE -> (Maybe FBAE)
evalS (Num x) = (Just (Num x))
evalS (Plus l r) = do { (Num l') <- (evalS l) ;
                        (Num r') <- (evalS r) ;
                        return (Num (l' + r')) }
evalS (Minus l r) = do { (Num l') <- (evalS l) ;
                         (Num r') <- (evalS r) ;
                         return (Num (l' - r')) }
evalS (Bind i v b) = do { v' <- evalS v ;
                          (evalS (subst i v' b)) }
evalS (Lambda i b) = return (Lambda i b)
evalS (App f a) = do { (Lambda i b) <- (evalS f) ;
                       a' <- (evalS a) ;
                       evalS (subst i a' b) }
evalS (If c t e) = do { (Num c') <- (evalS c) ;
                        if c'==0 then (evalS t) else (evalS e) }
evalS (Id id) = Nothing

interps = evalS . parseFBAE

-- Interpreter (Dynamic Scoping)

type Env = [(String,FBAE)]
         
evalM :: Env -> FBAE -> (Maybe FBAE)
evalM env (Num x) = return (Num x)
evalM env (Plus l r) = do { (Num l') <- (evalM env l) ;
                            (Num r') <- (evalM env r) ;
                            return (Num (l'+r')) }
evalM env (Minus l r) = do { (Num l') <- (evalM env l) ;
                             (Num r') <- (evalM env r) ;
                             return (Num (l'-r')) }
evalM env (Bind i v b) = do { v' <- evalM env v ;
                              evalM ((i,v'):env) b }
evalM env (Lambda i b) = return (Lambda i b)
evalM env (App f a) = do { (Lambda i b) <- (evalM env f) ;
                           a' <- (evalM env a) ;
                           evalM ((i,a'):env) b }
evalM env (Id id) = do { v <- (lookup id env) ;
                         return v }
evalM env (If c t e) = do { (Num c') <- (evalM env c) ;
                            if c'==0 then (evalM env t) else (evalM env e) }


interp = (evalM []) .  parseFBAE


-- Testing (Requires QuickCheck 2)

test1 = interp "(bind n = 1 in (bind f = (lambda x in x+n) in (bind n = 2 in app f 1)))"

test2 = let expr = "(bind n = 1 in (bind f = (lambda x in x+n) in (bind n = 2 in app f 1)))"
        in interp expr == interps expr

