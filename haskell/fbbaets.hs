{-# LANGUAGE GADTs,FlexibleContexts #-}

import Text.ParserCombinators.Parsec
import Control.Monad
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Calculator language extended with an environment to hold defined variables

data TFBAE = TNum | TBool | TFBAE :->: TFBAE | TLoc deriving (Show,Eq)

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
          | New FBAE
          | Set FBAE FBAE
          | Deref FBAE
          | Seq FBAE FBAE
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
appExpr = do f <- expr
             a <- expr
             return (App f a)

-- reserved "app"

term = bindExpr
       <|> lambdaExpr
       <|> parens appExpr
       <|> numExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr
       <|> identExpr
       <|> parens expr
 

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

type EnvS = [(String,FBAEVal)]
type ContS = [(String,TFBAE)]

data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> TFBAE -> FBAE -> EnvS -> FBAEVal
  LocV :: Int -> FBAEVal
  deriving (Show,Eq)

type Mem = Int -> Maybe FBAEVal
type Sto = (Int,Mem)

initMem :: Mem
initMem x = Nothing

setMem :: Int -> Mem -> FBAEVal -> Mem
setMem x m v = \z -> if z==x then (Just v) else m z

initSto :: Sto
initSto = (0,initMem)

setLoc :: Int -> Sto -> FBAEVal -> Sto
setLoc x (i,m) v = (i,setMem x m v)

newLoc :: Sto -> FBAEVal -> Sto
newLoc (i,m) v = ((i+1),(setMem i m v))

openLoc :: Int -> Sto -> Maybe FBAEVal
openLoc x (i,m) = m x

type RVal = Maybe (Sto,FBAEVal)

evalM :: EnvS -> Sto -> FBAE -> RVal
evalM env sto (Num x) = return (sto,(NumV x))
evalM env sto (Plus l r) = do { (sto',(NumV l')) <- (evalM env sto l) ;
                               (sto'',(NumV r')) <- (evalM env sto' r) ;
                               return (sto'',(NumV (l'+r'))) }
evalM env sto (Minus l r) = do { (sto',(NumV l')) <- (evalM env sto l) ;
                                (sto'',(NumV r')) <- (evalM env sto' r) ;
                                return (sto'',(NumV (l'-r'))) }
evalM env sto (Mult l r) = do { (sto',(NumV l')) <- (evalM env sto l) ;
                               (sto'',(NumV r')) <- (evalM env sto' r) ;
                               return (sto'',(NumV (l'*r'))) }
evalM env sto (Div l r) = do { (sto',(NumV l')) <- (evalM env sto l) ;
                              (sto'',(NumV r')) <- (evalM env sto' r) ;
                              return (sto'',(NumV (div l' r'))) }
evalM env sto (Bind i v b) = do { (sto',v') <- (evalM env sto v) ;
                                 evalM ((i,v'):env) sto' b }
evalM env sto (Lambda i t b) = return (sto,(ClosureV i t b env))
evalM env sto (App f a) = do { (sto',(ClosureV i t b e)) <- (evalM env sto f) ;
                              (sto'',a') <- (evalM env sto' a) ;
                              (evalM ((i,a'):e) sto'' b) }
evalM env sto (Id id) = case (lookup id env) of
                         Just x -> return (sto,x)
                         Nothing -> error "Varible not found"
evalM env sto (Boolean b) = return (sto,(BooleanV b))
evalM env sto (And l r) = do { (sto',(BooleanV l')) <- (evalM env sto l) ;
                              (sto'',(BooleanV r')) <- (evalM env sto' r) ;
                              return (sto',(BooleanV (l' && r'))) }
evalM env sto (Or l r) = do { (sto',(BooleanV l')) <- (evalM env sto l) ;
                             (sto'',(BooleanV r')) <- (evalM env sto' r) ;
                             return (sto',(BooleanV (l' || r'))) }
evalM env sto (Leq l r) = do { (sto',(NumV l')) <- (evalM env sto l) ;
                              (sto'',(NumV r')) <- (evalM env sto' r) ;
                              return (sto',(BooleanV (l' <= r'))) }
evalM env sto (IsZero v) = do { (sto',(NumV v')) <- (evalM env sto v) ;
                               return (sto',(BooleanV (v' == 0))) }
evalM env sto (If c t e) = do { (sto',(BooleanV c')) <- (evalM env sto c) ;
                               (if c'
                                 then (evalM env sto' t)
                                 else (evalM env sto' e)) }
evalM env sto (New t) = do { ((i,m),v) <- (evalM env sto t) ;
                             return ((newLoc (i,m) v),(LocV i)) }
evalM env sto (Set l v) = do { (sto',(LocV l')) <- (evalM env sto l) ;
                               (sto'',v') <- (evalM env sto' v) ;
                               return ((setLoc l' sto'' v'),v') }
evalM env sto (Deref l) = do { (sto',(LocV l')) <- (evalM env sto l) ;
                               return (case (openLoc l' sto') of
                                         Just v -> (sto',v)
                                         Nothing -> error "undefined location" ) } ;
evalM env sto (Seq l r) = do { (sto',_) <- (evalM env sto l) ;
                              (evalM env sto' r) }

-- Simple function that evaluates a term with an empty memory and
-- environment, drops the state, and prints the resulting value.
evalMi t = do { (s,v) <- evalM [] initSto t ;
                return v }

dumps s = let Just ((i,m),v) = s in dumpm i m

dumpm i m = do { print (m (i-1)) ; if i>=0 then dumpm (i-1) m else print "done"}

-- Examples for testing
ex1 = (Bind "x" (New (Num 3))
       (Seq (Set (Id "x")
             (Plus (Num 1) (Deref (Id "x")))) (Deref (Id "x"))))

-- Type Checker has not been updated to include state.

typeof :: ContS -> FBAE -> TFBAE
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
typeof cont (New t) = TLoc
typeof cont (Set l v) = if (typeof cont l)==TLoc
                        then (typeof cont v)
                        else error "Type error in Set"
typeof cont (Deref l) = if (typeof cont l)==TLoc
                        then TLoc
                        else error "bad dereferenced type"
typeof cont (Seq l r) = typeof cont r ;


intSta :: String -> RVal
intSta e = let p=(parseFBAE e) in
           let t=(typeof [] p) in
             if (t==TNum) || (t==TBool)
             then (evalM [] initSto p)
             else error "This should never happen"
