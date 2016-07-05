{-# LANGUAGE GADTs #-}

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

import ParserUtils

-- Calculator language extended with an environment to hold defined variables

data BAE where
  Num :: Int -> BAE
  Plus :: BAE -> BAE -> BAE
  Minus :: BAE -> BAE -> BAE
  Bind :: String -> BAE -> BAE -> BAE
  Id :: String -> BAE
  deriving (Show,Eq)

-- Parser

expr :: Parser BAE
expr = buildExpressionParser operators term

operators = [ [ inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft ]
            ]

numExpr :: Parser BAE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

identExpr :: Parser BAE
identExpr = do i <- identifier lexer
               return (Id i)

bindExpr :: Parser BAE
bindExpr = do reserved lexer "bind"
              i <- identifier lexer
              reservedOp lexer "="
              v <- expr
              reserved lexer "in"
              e <- expr
              return (Bind i v e)

term = parens lexer expr
       <|> numExpr
       <|> identExpr
       <|> bindExpr

parseBAE = parseString expr

parseBAEFile = parseFile expr

-- Substitution

subst :: String -> BAE -> BAE -> BAE
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Bind i' v' b') = if i==i'
                            then (Bind i' (subst i v v') b')
                            else (Bind i' (subst i v v') (subst i v b'))
subst i v (Id i') = if i==i'
                    then v
                    else (Id i')
       
calcs :: BAE -> Int
calcs (Num x) = x
calcs (Plus l r) = (calcs l) + (calcs r)
calcs (Minus l r) = (calcs l) - (calcs r)
calcs (Bind i v b) = (calcs (subst i (Num (calcs v)) b))
calcs (Id id) = error "Undeclared Variable"

-- Evaluation

type Env = [(String,Int)]
    
calc :: BAE -> Env -> Int
calc (Num x) env = x
calc (Plus l r) env = (calc l env) + (calc r env)
calc (Minus l r) env = (calc l env) - (calc r env)
calc (Bind i v b) env =
  let v' = calc v env in
    calc b ((i,v'):env)
calc (Id id) env = case (lookup id env) of
                     Just x -> x
                     Nothing -> error "Varible not found"
                                            

interp = calc . parseBAE
