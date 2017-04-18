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
          | Newbox FBAE
          | Setbox FBAE FBAE
          | Openbox FBAE
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

initSto :: (Sto)
initSto = (0,initMem)

setLoc :: Int -> Sto -> FBAEVal -> Sto
setLoc x (i,m) v = (i,setMem x m v)

newLoc :: Sto -> FBAEVal -> Sto
newLoc (i,m) v = ((i+1),(setMem i m v))

openLoc :: Int -> Sto -> Maybe FBAEVal
openLoc x (i,m) = m x

type RVal = (Sto,FBAEVal)

eSta :: EnvS -> Sto -> FBAE -> RVal
eSta env sto (Num x) = (sto,(NumV x))
eSta env sto (Plus l r) = let (sto',(NumV l')) = (eSta env sto l)
                              (sto'',(NumV r')) = (eSta env sto' r)
                          in (sto'',(NumV (l'+r')))
eSta env sto (Minus l r) = let (sto',(NumV l')) = (eSta env sto l)
                               (sto'',(NumV r')) = (eSta env sto' r)
                           in (sto'',(NumV (l'-r')))
eSta env sto (Mult l r) = let (sto',(NumV l')) = (eSta env sto l)
                              (sto'',(NumV r')) = (eSta env sto' r)
                          in (sto'',(NumV (l'*r')))
eSta env sto (Div l r) = let (sto',(NumV l')) = (eSta env sto l)
                             (sto'',(NumV r')) = (eSta env sto' r)
                          in (sto'',(NumV (div l' r')))
eSta env sto (Bind i v b) = let (sto',v') = eSta env sto v in
                              eSta ((i,v'):env) sto' b
eSta env sto (Lambda i t b) = (sto,(ClosureV i t b env))
eSta env sto (App f a) = let (sto',(ClosureV i t b e)) = (eSta env sto f)
                             (sto'',a') = (eSta env sto' a)
                         in eSta ((i,a'):e) sto'' b
eSta env sto (Id id) = case (lookup id env) of
                         Just x -> (sto,x)
                         Nothing -> error "Varible not found"
eSta env sto (Boolean b) = (sto,(BooleanV b))
eSta env sto (And l r) = let (sto',(BooleanV l')) = (eSta env sto l)
                             (sto'',(BooleanV r')) = (eSta env sto' r)
                         in (sto',(BooleanV (l' && r')))
eSta env sto (Or l r) = let (sto',(BooleanV l')) = (eSta env sto l)
                            (sto'',(BooleanV r')) = (eSta env sto' r)
                        in (sto',(BooleanV (l' || r')))
eSta env sto (Leq l r) = let (sto',(NumV l')) = (eSta env sto l)
                             (sto'',(NumV r')) = (eSta env sto' r)
                         in (sto',(BooleanV (l' <= r')))
eSta env sto (IsZero v) = let (sto',(NumV v')) = (eSta env sto v)
                          in (sto',(BooleanV (v' == 0)))
eSta env sto (If c t e) = let (sto',(BooleanV c')) = (eSta env sto c)
                          in if c' then (eSta env sto' t) else (eSta env sto' e)
eSta env sto (Newbox t) = let (s,v) = (eSta env sto t)
                          in ((newLoc s v),v)
eSta env sto (Setbox l v) = let (sto',(LocV l')) = (eSta env sto l)
                                (sto'',v') = (eSta env sto' v)
                            in ((setLoc l' sto'' v'),v')
eSta env sto (Openbox l) = let (sto',(LocV l')) = (eSta env sto l)
                           in case (openLoc l' sto') of
                                Just v -> (sto',v)
                                Nothing -> error "undefined location"
eSta env sto (Seq l r) = let (sto',_) = (eSta env sto l)
                         in (eSta env sto' r)

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


intSta :: String -> (Sto,FBAEVal)
intSta e = let p=(parseFBAE e) in
           let t=(typeof [] p) in
             if (t==TNum) || (t==TBool)
             then (eSta [] initSto p)
             else error "This should never happen"
