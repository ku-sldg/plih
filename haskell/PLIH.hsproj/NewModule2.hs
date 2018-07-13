{-# LANGUAGE GADTs,FlexibleContexts #-}

module Fbbaets where

import Text.ParserCombinators.Parsec
import Control.Monad
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Expression language with first class functions and static scoping
-- extended to include mutable store.

-- AST for types
data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  TLoc :: TFBAE -> TFBAE
  deriving (Show,Eq)

-- AST for terms
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
  New :: FBAE -> FBAE
  Set :: FBAE -> FBAE -> FBAE
  Deref :: FBAE -> FBAE
  Seq :: FBAE -> FBAE -> FBAE
  deriving (Show,Eq)

-- Term keywords
names = [ "lambda"
        , "bind"
        , "in"
        , "if"
        , "then"
        , "else"
        , "isZero"
        , "true"
        , "false"
        , "new"
        , "deref"
        , "set"
        , "Num"
        , "Bool"
        , "Loc"
        ]

-- Operator symbols
ops = [ "+","-","*","/","&&","||","<=","=",":","->",";",":=","!"]

-- Token lexer definition
tokenDef =
  emptyDef { Token.commentLine = "--"
           , Token.identStart = letter
           , Token.identLetter = alphaNum <|> char '_'
           , Token.reservedNames = names
           , Token.reservedOpNames = ops
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


-- Treating whitespace as an operator.  I guess that's one way to parse
-- applications.
appl = Infix space AssocLeft
  where space = whiteSpace
                *> notFollowedBy (choice . map reservedOp $ ops)
                *> return (\x y -> App x y)

-- Shorthand function for defining infix, binary operations
binary name label assoc = Infix (do { reservedOp name ;
                                      return (\x y -> label x y)
                                    }) assoc

-- Shorthand function for defining prefix, unary operations
prefix name label = Prefix (reservedOp name *> return (\x -> label x))


-- Operations list
operators = [ [ appl ]
            , [ binary "*" Mult AssocLeft,
                binary "/" Div AssocLeft ]
            , [ binary "+" Plus AssocLeft,
                binary "-" Minus AssocLeft ]
            , [ binary "&&" And AssocLeft,
                binary "||" Or AssocLeft]
            , [ binary "<=" Leq AssocLeft ]
            , [ prefix "isZero" IsZero ]
            , [ prefix "!" Deref,
                prefix "new" New ,
                binary ":=" Set AssocLeft ]
            , [ binary ";" Seq AssocLeft ]
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

setExpr :: Parser FBAE
setExpr = do reserved "set"
             l <- expr
             reservedOp "="
             v <- expr
             return (Set l v)
             

term = bindExpr
       <|> lambdaExpr
       <|> numExpr
       <|> trueExpr
       <|> falseExpr
       <|> setExpr
       <|> ifExpr
       <|> identExpr
       <|> parens expr

-- Type parser

ty = buildExpressionParser tyoperators tyTerm

tyoperators = [ [ prefix "Loc" TLoc ]
                , [ binary "->" (:->:) AssocLeft ] ]

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

type Env = [(String,FBAEVal)]
type Cont = [(String,TFBAE)]

data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> TFBAE -> FBAE -> Env -> FBAEVal
  LocV :: Int -> FBAEVal
  deriving (Show,Eq)

-- Model memmory as a function from int to value returning Nothiung when
-- memory location as not been initialized
type Mem = Int -> Maybe FBAEVal
-- Model the store as a memory and a last allocated counter.  Counter should
-- be upated by new.
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

evalM :: Env -> Sto -> FBAE -> RVal
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
                               (case (openLoc l' sto') of
                                         Just v -> return (sto',v)
                                         Nothing -> Nothing) } ;
evalM env sto (Seq l r) = do { (sto',_) <- (evalM env sto l) ;
                              (evalM env sto' r) }

-- Simple function that evaluates a term with an empty memory and
-- environment, drops the state, and prints the resulting value.
evalMi t = do { (s,v) <- evalM [] initSto t ;
                return v }

-- Simple function to peek in a memory location
peek s x = let Just ((i,m),v) = s in print (m x)

-- Examples for testing
ex1 = (Bind "x" (New (Num 3))
       (Seq
         (Set (Id "x") (Plus (Num 1) (Deref (Id "x"))))
         (Deref (Id "x"))))

ex1s = "bind x = (new 3) in x := (1 + (!x)) ; !x"

ex2 = (Bind "f" (New (Lambda "x" TNum (Plus (Id "x") (Num 1))))
        (App (Deref (Id "f")) (Num 1)))

ex2s = "bind f = (new (lambda (x:Nat) in x+1)) in ((!f) 1)"

-- Type Checker uses a typed location.

typeofM :: Cont -> FBAE -> (Maybe TFBAE)
typeofM cont (Num x) = return TNum
typeofM cont (Plus l r) = do { TNum <- (typeofM cont l) ;
                               TNum <- (typeofM cont r) ;
                               return TNum }
typeofM cont (Minus l r) = do { TNum <- (typeofM cont l) ;
                                TNum <- (typeofM cont r) ;
                                return TNum }
typeofM cont (Mult l r) = do { TNum <- (typeofM cont l) ;
                               TNum <- (typeofM cont r) ;
                               return TNum }
typeofM cont (Div l r) = do { TNum <- (typeofM cont l) ;
                              TNum <- (typeofM cont r) ;
                              return TNum }
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
                             TBool <- (typeofM cont r);
                             return TBool }
typeofM cont (Leq l r) = do { TNum <- (typeofM cont l) ;
                              TNum <- (typeofM cont r) ;
                              return TBool }
typeofM cont (IsZero v) = do { TNum <- (typeofM cont v) ;
                               return TBool }
typeofM cont (If c t e) = do { TBool <- (typeofM cont c) ;
                               t' <- (typeofM cont t) ;
                               e' <- (typeofM cont e) ;
                               if t' == e'
                               then return t'
                               else Nothing }
typeofM cont (New t) = do { t' <- (typeofM cont t) ;
                            return (TLoc t') }
typeofM cont (Set l v) = do { (TLoc l') <- (typeofM cont l) ;
                              v' <- (typeofM cont v) ;
                              if l'==v'
                              then return v'
                              else Nothing }
typeofM cont (Deref l) = do { (TLoc l') <- (typeofM cont l) ;
                              return l'}
typeofM cont (Seq l r) = do { (typeofM cont l) ;
                              (typeofM cont r) }


intM :: String -> RVal
intM e = let p=(parseFBAE e) in
           let t=(typeofM [] p) in
             if (t==(Just TNum)) || (t==(Just TBool))
             then (evalM [] initSto p)
             else Nothing

intMi :: String -> Maybe FBAEVal
intMi e = let p=(parseFBAE e) in
            do { t<-(typeofM [] p) ;
                 (evalMi p) }



