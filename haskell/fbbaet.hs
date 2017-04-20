{-# LANGUAGE GADTs #-}

import Text.ParserCombinators.Parsec
import Control.Monad
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Calculator language extended with an environment to hold defined variables

data TFBAE = TNum | TBool | TFBAE :->: TFBAE deriving (Show,Eq)

data FBAE = Num Int
          | Plus FBAE FBAE
          | Minus FBAE FBAE
          | Mult FBAE FBAE
          | Div FBAE FBAE
          | Bind String FBAE FBAE
          | Lambda String TFBAE FBAE
          | App FBAE FBAE
          | Id String
          | Boolean Bool
          | And FBAE FBAE
          | Or FBAE FBAE
          | Leq FBAE FBAE
          | IsZero FBAE
          | If FBAE FBAE FBAE
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
                                    , "false" ]
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

term = parens expr
       <|> numExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr
       <|> identExpr
       <|> bindExpr
       <|> lambdaExpr
       <|> appExpr


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


type Env = [(String,FBAE)]
type Cont = [(String,TFBAE)]
         
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

type EnvS = [(String,FBAEVal)]
type ContS = [(String,TFBAE)]

data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> TFBAE -> FBAE -> EnvS -> FBAEVal
  deriving (Show,Eq)

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


intDyn :: String -> FBAE
intDyn e = let p=(parseFBAE e) in
           let t=(typeof [] p) in
             if (t==TNum) || (t==TBool)
             then (eDyn [] p)
             else error "This should never happen"

intSta :: String -> FBAEVal
intSta e = let p=(parseFBAE e) in
           let t=(typeof [] p) in
             if (t==TNum) || (t==TBool)
             then (eSta [] p)
             else error "This should never happen"
