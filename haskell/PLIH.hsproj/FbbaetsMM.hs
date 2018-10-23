{-# LANGUAGE GADTs,FlexibleContexts #-}

module FbbaetsMM where

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

-- Reader defines local memory
newtype Reader e a = Reader { runR :: e -> a }

instance Functor (Reader e) where
  fmap f (Reader g) = Reader $ \e -> (f . g) e

instance Applicative (Reader e) where
  pure x = Reader $ \e -> x
  (Reader f) <*> (Reader g) = Reader $ \e -> (f e) (g e)

instance Monad (Reader e) where
  return x = Reader $ \e -> x
  g >>= f = Reader $ \e -> runR (f (runR g e)) e

-- ask simply returns e
ask :: Reader a a
ask = Reader $ \e -> e

-- asks applies a function to e and returns it
asks :: (e -> a) -> Reader e a
asks f = ask >>= \e -> (return (f e))

-- local makes local changes to e 
local :: (e -> t) -> Reader t a -> Reader e a
local f r = ask >>= \e -> return (runR r (f e))

-- explicit stores a specified environment as e
explicit :: e -> Reader e a -> Reader e a
explicit e r = return (runR r e)


-- State encapsulates a function from some state to an output and
-- a new state.

newtype St s a = St { runS :: s -> (a , s) }

instance Monad (St s) where
  return x = St $ \s -> (x,s)
  p >>= k = St $ \s0 ->
    let (x,s1) = runS p s0 in
      runS (k x) s1

instance Functor (St s) where
  fmap = Control.Monad.liftM
  
instance Applicative (St s) where
  pure = return
  (<*>) = Control.Monad.ap
  

-- Make s the next state and provide an output value
put s = St $ \x -> (x,s)

-- Make the state the next output
get = St $ \s -> (s,s)

-- Update the state by applying a function to an input state
update f = St $ \s -> (s,(f s))

-- Apply a function to state and return the result
gets f = St $ \s -> ((f s),s)

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

type RVal = St Sto FBAEVal

evalM :: Env -> FBAE -> RVal
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
evalM env (Bind i v b) = do { v' <- (evalM env v) ;
                              evalM ((i,v'):env) b }
evalM env (Lambda i t b) = return (ClosureV i t b env)
evalM env (App f a) = do { (ClosureV i t b e) <- (evalM env f) ;
                           a' <- (evalM env a) ;
                           (evalM ((i,a'):e) b) }
evalM env (Id id) = case (lookup id env) of
                         Just x -> return x
                         Nothing -> error "Varible not found"
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
                            (if c'
                             then (evalM env t)
                             else (evalM env e)) }
evalM env (New t) = do { v <- (evalM env t) ;
                         (i,m) <- get ;
                         put (newLoc (i,m) v) ;
                         return (LocV i) }
evalM env (Set l v) = do { (LocV l') <- (evalM env l) ;
                           v' <- (evalM env v) ;
                           s <- get ;
                           put (setLoc l' s v') ;
                           return v' }
evalM env (Deref l) = do { (LocV l') <- (evalM env l) ;
                           s <- get ;
                           (case (openLoc l' s) of
                                    Just v -> return v
                                    Nothing -> error "Null location") } ;
evalM env (Seq l r) = do { (evalM env l) ;
                           (evalM env r) }

-- Simple function that evaluates a term with an empty memory and
-- environment, drops the state, and prints the resulting value.
evalMi t = let (v,s) = runS (evalM [] t) initSto in v


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
             then (evalM [] p)
             else error "Bad Result"

intMi :: String -> Maybe FBAEVal
intMi e = let p=(parseFBAE e) in
            do { t <- (typeofM [] p) ;
                 return (evalMi p) }



