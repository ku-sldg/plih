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
         
eDyn :: Env -> FBAE -> (Either String FBAE)
eDyn env (Num x) = (Right (Num x))
eDyn env (Plus l r) = do { (Num l') <- (eDyn env l) ;
                           (Num r') <- (eDyn env r) ;
                           return (Num (l'+r'))
                         }
eDyn env (Minus l r) = do { (Num l') <- (eDyn env l) ;
                            (Num r') <- (eDyn env r) ;
                            return (Num (l'-r')) }
eDyn env (Mult l r) = do { (Num l') <- (eDyn env l) ;
                           (Num r') <- (eDyn env r) ;
                           return (Num (l'*r')) }
eDyn env (Div l r) = do { (Num l') <- (eDyn env l) ;
                          (Num r') <- (eDyn env r) ;
                          return (Num (div l' r')) }
eDyn env (Bind i v b) = do { v' <- eDyn env v ;
                             eDyn ((i,v'):env) b }
eDyn env (Lambda i t b) = return (Lambda i t b)
eDyn env (App f a) = do { (Lambda i t b) <- (eDyn env f) ;
                          a' <- (eDyn env a) ;
                          eDyn ((i,a'):env) b }
eDyn env (Id id) = case (lookup id env) of
                     Just x -> (Right x)
                     Nothing -> (Left "Varible not found")
eDyn env (Boolean b) = return (Boolean b)
eDyn env (And l r) = do { (Boolean l') <- (eDyn env l) ;
                          (Boolean r') <- (eDyn env r) ;
                          return (Boolean (l' && r')) }
eDyn env (Or l r) = do { (Boolean l') <- (eDyn env l) ;
                         (Boolean r') <- (eDyn env r) ;
                         return (Boolean (l' || r')) }
eDyn env (Leq l r) = do { (Num l') <- (eDyn env l) ;
                         (Num r') <- (eDyn env r) ;
                         return (Boolean (l' <= r')) }
eDyn env (IsZero v) = do { (Num v') <- (eDyn env v) ;
                           return (Boolean (v' == 0)) }
eDyn env (If c t e) = do { (Boolean c') <- (eDyn env c) ; 
                            if c' then (eDyn env t) else (eDyn env e) }

type EnvS = [(String,FBAEVal)]
type ContS = [(String,TFBAE)]

data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> TFBAE -> FBAE -> EnvS -> FBAEVal
  deriving (Show,Eq)

eSta :: EnvS -> FBAE -> (Either String FBAEVal)
eSta env (Num x) = (Right (NumV x))
eSta env (Plus l r) = do { (NumV l') <- (eSta env l) ;
                           (NumV r') <- (eSta env r) ;
                           return (NumV (l'+r')) }
eSta env (Minus l r) = do { (NumV l') <- (eSta env l) ;
                            (NumV r') <- (eSta env r) ;
                            return (NumV (l'-r')) }
eSta env (Mult l r) = do { (NumV l') <- (eSta env l) ;
                           (NumV r') <- (eSta env r) ;
                           return (NumV (l'*r')) }
eSta env (Div l r) = do { (NumV l') <- (eSta env l) ;
                          (NumV r') <- (eSta env r) ;
                          return (NumV (div l' r')) }
eSta env (Bind i v b) = do { v' <- eSta env v ;
                             eSta ((i,v'):env) b }
eSta env (Lambda i t b) = return (ClosureV i t b env)
eSta env (App f a) = do { (ClosureV i t b e) <- (eSta env f) ;
                          a' <- (eSta env a) ;
                          (eSta ((i,a'):e) b) }
eSta env (Id id) = case (lookup id env) of
                     Just x -> (Right x)
                     Nothing -> (Left "Varible not found")
eSta env (Boolean b) = return (BooleanV b)
eSta env (And l r) = do { (BooleanV l') <- (eSta env l) ;
                          (BooleanV r') <- (eSta env r) ;
                          return (BooleanV (l' && r')) }
eSta env (Or l r) = do { (BooleanV l') <- (eSta env l) ;
                         (BooleanV r') <- (eSta env r) ;
                         return (BooleanV (l' || r')) }
eSta env (Leq l r) = do { (NumV l') <- (eSta env l) ;
                          (NumV r') <- (eSta env r) ;
                          return (BooleanV (l' <= r')) }
eSta env (IsZero v) = do { (NumV v') <- (eSta env v) ;
                           return (BooleanV (v' == 0)) }
eSta env (If c t e) = do { (BooleanV c') <- (eSta env c) ;
                           (if c' then (eSta env t) else (eSta env e)) }


typeof :: Cont -> FBAE -> (Either String TFBAE)
typeof cont (Num x) = return TNum
typeof cont (Plus l r) = do { l' <- (typeof cont l) ;
                              r' <- (typeof cont r) ;
                              if l'==TNum && r'==TNum
                              then return TNum
                              else (Left "Type Mismatch in +")}
typeof cont (Minus l r) = do { l' <- (typeof cont l) ;
                               r' <- (typeof cont r) ;
                               if l'==TNum && r'==TNum
                               then return TNum else Left "Type Mismatch in -" }
typeof cont (Mult l r) = do { l' <- (typeof cont l) ;
                              r' <- (typeof cont r) ;
                              if l'==TNum && r'==TNum
                              then return TNum
                              else Left "Type Mismatch in *" }
typeof cont (Div l r) = do { l' <- (typeof cont l) ;
                             r' <- (typeof cont r) ;
                             if l'==TNum && r'==TNum
                             then return TNum
                             else Left "Type Mismatch in /" }
typeof cont (Bind i v b) = do { v' <- typeof cont v ;
                                typeof ((i,v'):cont) b }
typeof cont (Id id) = case (lookup id cont) of
                        Just x -> (Right x)
                        Nothing -> (Left "Varible not found")
typeof cont (Lambda x t b) = do { tyB <- typeof ((x,t):cont) b ;
                                  return (t :->: tyB) }
typeof cont (App x y) = do { tyXd :->: tyXr <- typeof cont x ;
                             tyY <- typeof cont y ;
                             if tyXd==tyY
                             then return tyXr
                             else Left "Type mismatch in app" }
typeof cont (Boolean b) = return TBool
typeof cont (And l r) = do { l' <- (typeof cont l) ;
                             r' <- (typeof cont r) ;
                             if l'== TBool && r' == TBool
                             then return TBool
                             else Left "Type mismatch in &&" }
typeof cont (Or l r) = do { l' <- (typeof cont l) ;
                            r' <- (typeof cont r) ;
                            if l' == TBool && r' == TBool
                            then return TBool
                            else Left "Type mismatch in ||" }
typeof cont (Leq l r) = do { l' <- (typeof cont l) ;
                             r' <- (typeof cont r) ;
                             if l'== TNum && r' == TNum
                             then return TBool
                             else Left "Type mismatch in <=" }
typeof cont (IsZero v) = do { v' <- (typeof cont v) ;
                              if v' == TNum
                              then return TBool
                              else Left "Type mismatch in IsZero" }
typeof cont (If c t e) = do { c' <- (typeof cont c) ;
                              t' <- (typeof cont t) ;
                              e' <- (typeof cont e) ;
                              if c' == TBool && t'==e'
                              then return t'
                              else (Left "Type mismatch in if") }

intDyn :: String -> (Either String FBAE)
intDyn e = let p=(parseFBAE e) in
           case (typeof [] p) of
             (Right _) -> (eDyn [] p)
             (Left x) -> (Left x)

intSta :: String -> (Either String FBAEVal)
intSta e = let p=(parseFBAE e) in
           case (typeof [] p) of
             (Right _) -> (eSta [] p)
             (Left x) -> (Left x)
