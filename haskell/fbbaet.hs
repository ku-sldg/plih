{-# LANGUAGE GADTs, FlexibleContexts #-}

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

tokenDef =
  javaStyle { Token.identStart = letter
            , Token.identLetter = alphaNum
            , Token.reservedNames = [ "lambda"
                                    , "bind"
                                    , "in"
                                    , "if"
                                    , "then"
                                    , "else"
                                    , "isZero"
                                    , "app"
                                    , "Num"
                                    , "Bool"
                                    , "true"
                                    , "false"
                                    , "fix" ]
            , Token.reservedOpNames = [ "+","-","*","/","&&","||","<=","=",":","->"]
            }

lexer = Token.makeTokenParser tokenDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer

-- Term parser

expr :: Parser FBAE
expr = buildExpressionParser operators term

operators = [ [Infix (reservedOp "*" >> return (Mult )) AssocLeft,
               Infix (reservedOp "/" >> return (Div )) AssocLeft ]
            , [Infix (reservedOp "+" >> return (Plus )) AssocLeft,
               Infix (reservedOp "-" >> return (Minus )) AssocLeft ]
            , [Infix (reservedOp "&&" >> return (And )) AssocLeft,
               Infix (reservedOp "||" >> return (Or )) AssocLeft]
            , [Infix (reservedOp "<=" >> return (Leq )) AssocLeft ]
            , [Prefix (reserved "isZero" >> return (IsZero )) ]
            ]

numExpr :: Parser FBAE
numExpr = do i <- integer
             return (Num (fromInteger i))

trueExpr :: Parser FBAE
trueExpr = do i <- reserved "true"
              return (Boolean True)

falseExpr :: Parser FBAE
falseExpr = do i <- reserved "false"
               return (Boolean False)

ifExpr :: Parser FBAE
ifExpr = do reserved "if"
            c <- expr
            reserved "then"
            t <- expr
            reserved "else"
            e <- expr
            return (If c t e)

identExpr :: Parser FBAE
identExpr = do i <- identifier
               return (Id i)

bindExpr :: Parser FBAE
bindExpr = do reserved "bind"
              i <- identifier
              reservedOp "="
              v <- expr
              reserved "in"
              e <- expr
              return (Bind i v e)

lambdaExpr :: Parser FBAE
lambdaExpr = do reserved "lambda"
                (i,t) <- parens argExpr
                reserved "in"
                b <- expr
                return (Lambda i t b)

argExpr :: Parser (String,TFBAE)
argExpr = do i <- identifier
             reservedOp ":"
             t <- ty
             return (i,t)

appExpr :: Parser FBAE
appExpr = do reserved "app"
             f <- expr
             a <- expr
             return (App f a)

fixExpr :: Parser FBAE
fixExpr = do reserved "fix"
             t <- expr
             return (Fix t)

term = parens expr
       <|> numExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr
       <|> identExpr
       <|> bindExpr
       <|> lambdaExpr
       <|> appExpr
       <|> fixExpr


-- Type parser

ty = buildExpressionParser tyoperators tyTerm

tyoperators = [ [Infix (reservedOp "->" >> return (:->: )) AssocLeft ] ]

tyTerm :: Parser TFBAE
tyTerm = parens ty <|> tyNat <|> tyBool

tyNat :: Parser TFBAE
tyNat = do reserved "Nat"
           return TNum

tyBool :: Parser TFBAE
tyBool = do reserved "Bool"
            return TBool

-- Parser invocation

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

parseFBAE = parseString expr

parseFile p file =
  do program <- readFile file
     case parse p "" program of
       Left e -> print e >> fail "parse error"
       Right r -> return r

parseFBAEFile = parseFile expr

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

-- Evaluation and Type Derivation

-- Enviornment for dynamically scoped eval

type Env = [(String,FBAE)]

-- Dynamically scoped eval

eDyn :: Env -> FBAE -> FBAE
eDyn env (Num x) = (Num x)
eDyn env (Plus l r) = let (Num l') = (eDyn env l)
                          (Num r') = (eDyn env r)
                      in (Num (l'+r'))
eDyn env (Minus l r) = let (Num l') = (eDyn env l)
                           (Num r') = (eDyn env r)
                       in (Num (l'-r'))
eDyn env (Mult l r) = let (Num l') = (eDyn env l)
                          (Num r') = (eDyn env r)
                      in (Num (l'*r'))
eDyn env (Div l r) = let (Num l') = (eDyn env l)
                         (Num r') = (eDyn env r)
                      in (Num (div l' r'))
eDyn env (Bind i v b) = let v' = eDyn env v in
                          eDyn ((i,v'):env) b
eDyn env (Lambda i t b) = (Lambda i t b)
eDyn env (App f a) = let (Lambda i t b) = (eDyn env f)
                         a' = (eDyn env a)
                     in eDyn ((i,a'):env) b
eDyn env (Id id) = case (lookup id env) of
                     Just x -> x
                     Nothing -> error "Varible not found"
eDyn env (Boolean b) = (Boolean b)
eDyn env (And l r) = let (Boolean l') = (eDyn env l)
                         (Boolean r') = (eDyn env r)
                      in (Boolean (l' && r'))
eDyn env (Or l r) = let (Boolean l') = (eDyn env l)
                        (Boolean r') = (eDyn env r)
                    in (Boolean (l' || r'))
eDyn env (Leq l r) = let (Num l') = (eDyn env l)
                         (Num r') = (eDyn env r)
                      in (Boolean (l' <= r'))
eDyn env (IsZero v) = let (Num v') = (eDyn env v)
                      in (Boolean (v' == 0))
eDyn env (If c t e) = let (Boolean c') = (eDyn env c)
                      in if c' then (eDyn env t) else (eDyn env e)
eDyn env (Fix t) = let (Lambda i ty b) = (eDyn env t) in
                     eDyn env (subst i (Fix (Lambda i ty b)) b)

-- Enviornment for statically scoped eval

type EnvS = [(String,FBAEVal)]

-- Value defintion for statically scoped eval

data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> TFBAE -> FBAE -> EnvS -> FBAEVal
  deriving (Show,Eq)

-- Statically scoped eval

eSta :: EnvS -> FBAE -> FBAEVal
eSta env (Num x) = (NumV x)
eSta env (Plus l r) = let (NumV l') = (eSta env l)
                          (NumV r') = (eSta env r)
                      in (NumV (l'+r'))
eSta env (Minus l r) = let (NumV l') = (eSta env l)
                           (NumV r') = (eSta env r)
                       in (NumV (l'-r'))
eSta env (Mult l r) = let (NumV l') = (eSta env l)
                          (NumV r') = (eSta env r)
                      in (NumV (l'*r'))
eSta env (Div l r) = let (NumV l') = (eSta env l)
                         (NumV r') = (eSta env r)
                      in (NumV (div l' r'))
eSta env (Bind i v b) = let v' = eSta env v in
                          eSta ((i,v'):env) b
eSta env (Lambda i t b) = (ClosureV i t b env)
eSta env (App f a) = let (ClosureV i t b e) = (eSta env f)
                         a' = (eSta env a)
                     in eSta ((i,a'):e) b
eSta env (Id id) = case (lookup id env) of
                     Just x -> x
                     Nothing -> error "Varible not found"
eSta env (Boolean b) = (BooleanV b)
eSta env (And l r) = let (BooleanV l') = (eSta env l)
                         (BooleanV r') = (eSta env r)
                      in (BooleanV (l' && r'))
eSta env (Or l r) = let (BooleanV l') = (eSta env l)
                        (BooleanV r') = (eSta env r)
                    in (BooleanV (l' || r'))
eSta env (Leq l r) = let (NumV l') = (eSta env l)
                         (NumV r') = (eSta env r)
                      in (BooleanV (l' <= r'))
eSta env (IsZero v) = let (NumV v') = (eSta env v)
                      in (BooleanV (v' == 0))
eSta env (If c t e) = let (BooleanV c') = (eSta env c)
                      in if c' then (eSta env t) else (eSta env e)
eSta env (Fix t) = let (ClosureV i ty b e) = (eSta env t) in
                     eSta e (subst i (Fix (Lambda i ty b)) b)


-- Type inference function

type Cont = [(String,TFBAE)]

typeof :: Cont -> FBAE -> TFBAE
typeof cont (Num x) = TNum
typeof cont (Plus l r) = let l' = (typeof cont l)
                             r' = (typeof cont r)
                         in if l'==TNum && r'==TNum
                            then TNum
                            else error "Type Mismatch in +"
typeof cont (Minus l r) = let l' = (typeof cont l)
                              r' = (typeof cont r)
                          in if l'==TNum && r'==TNum then TNum else error "Type Mismatch in -"
typeof cont (Mult l r) = let l' = (typeof cont l)
                             r' = (typeof cont r)
                         in if l'==TNum && r'==TNum
                            then TNum
                            else error "Type Mismatch in *"
typeof cont (Div l r) = let l' = (typeof cont l)
                            r' = (typeof cont r)
                        in if l'==TNum && r'==TNum
                           then TNum
                           else error "Type Mismatch in /"
typeof cont (Bind i v b) = let v' = typeof cont v in
                             typeof ((i,v'):cont) b
typeof cont (Id id) = case (lookup id cont) of
                        Just x -> x
                        Nothing -> error "Varible not found"
typeof cont (Lambda x t b) = let tyB = typeof ((x,t):cont) b
                             in t :->: tyB
typeof cont (App x y) = let tyXd :->: tyXr = typeof cont x
                            tyY = typeof cont y
                        in if tyXd==tyY
                           then tyXr
                           else error "Type mismatch in app"
typeof cont (Boolean b) = TBool
typeof cont (And l r) = if (typeof cont l) == TBool && (typeof cont r) == TBool
                        then TBool
                        else error "Type mismatch in &&"
typeof cont (Or l r) = if (typeof cont l) == TBool && (typeof cont r) == TBool
                       then TBool
                       else error "Type mismatch in ||"
typeof cont (Leq l r) = if (typeof cont l) == TNum && (typeof cont r) == TNum
                        then TBool
                        else error "Type mismatch in <="
typeof cont (IsZero v) = if (typeof cont v) == TNum
                         then TBool
                         else error "Type mismatch in IsZero"
typeof cont (If c t e) = if (typeof cont c) == TBool
                            && (typeof cont t)==(typeof cont e)
                         then (typeof cont t)
                         else error "Type mismatch in if"
typeof cont (Fix t) = let r:->:d = typeof cont t
                      in d

-- Elaborator written for testing

elab :: FBAE -> FBAE
elab (Num x) = (Num x)
elab (Plus l r) = let l' = (elab l)
                      r' = (elab r)
                  in (Plus l' r')
elab (Minus l r) = let l' = (elab l)
                       r' = (elab r)
                   in (Minus l' r')
elab (Mult l r) = let l' = (elab l)
                      r' = (elab r)
                  in (Mult l' r')
elab (Div l r) = let l' = (elab l)
                     r' = (elab r)
                 in (Div l' r')
elab (Bind i v b) = let v' = elab v
                        b' = elab b
                    in (App (Lambda i TNum b') v')
elab (Lambda i t b) = (Lambda i t b)
elab (App f a) = (App (elab f) (elab a))
elab (Id id) = (Id id)
elab (Boolean b) = (Boolean b)
elab (And l r) = let l' = (elab l)
                     r' = (elab r)
                 in (And l' r')
elab (Or l r) = let l' = (elab l)
                    r' = (elab r)
                in (Or l' r')
elab (Leq l r) = let l' = (elab l)
                     r' = (elab r)
                 in (Leq l' r')
elab (IsZero v) = let v' = (elab v)
                  in (IsZero v')
elab (If c t e) = If (elab c) (elab t) (elab e)
elab (Fix t) = (Fix (elab t))

-- Example recursive function for summation.  Run it using intDyn or intSta

ffs = "app (fix (lambda (ie:Nat) in (lambda (x:Nat) in if (isZero x) then x else x + app ie x - 1))) 5"

intDyn :: String -> FBAE
intDyn e = let p=(parseFBAE e) in
           let t=(typeof [] p) in
             (eDyn [] p)

intSta :: String -> FBAEVal
intSta e = let p=(parseFBAE e) in
           let t=(typeof [] p) in
             (eSta [] p)

