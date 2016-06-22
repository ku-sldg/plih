{-# LANGUAGE GADTs #-}

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

import ParserUtils

-- Calculator language extended with type derivation function

data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving (Show,Eq)

data ABE where
  Num :: Int -> ABE
  Plus :: ABE -> ABE -> ABE
  Minus :: ABE -> ABE -> ABE
  Boolean :: Bool -> ABE
  And :: ABE -> ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
  If :: ABE -> ABE -> ABE -> ABE
  deriving (Show,Eq)
                    
-- Parser

expr :: Parser ABE
expr = buildExpressionParser operators term

operators = [ [ inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft ]
            , [ inFix "&&" And AssocLeft ]
            , [ inFix "<=" Leq AssocLeft ]
            , [ preFix "isZero" IsZero ]
            ]

numExpr :: Parser ABE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

trueExpr :: Parser ABE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser ABE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)

ifExpr :: Parser ABE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)

term = parens lexer expr
       <|> numExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr

-- Parser invocation

parseABE = parseString expr

parseABEFile = parseFile expr

-- Interpreter

eval :: ABE -> ABE
eval (Num x) = (Num x)
eval (Plus l r) = let (Num l') = (eval l)
                      (Num r') = (eval r)
                      in (Num (l'+r'))
eval (Minus l r) = let (Num l') = (eval l)
                       (Num r') = (eval r)
                       in (Num (l'-r'))
eval (Boolean b) = (Boolean b)
eval (And l r) = let (Boolean l') = (eval l)
                     (Boolean r') = (eval r)
                 in (Boolean (l' && r'))
eval (Leq l r) = let (Num l') = (eval l)
                     (Num r') = (eval r)
                 in (Boolean (l' <= r'))
eval (IsZero v) = let (Num v') = (eval v)
                  in (Boolean (v' == 0))
eval (If c t e) = let (Boolean c') = (eval c)
                  in if c' then (eval t) else (eval e)


typeof :: ABE -> TABE
typeof (Num x) = TNum
typeof (Plus l r) = let l' = (typeof l)
                        r' = (typeof r)
                    in if l'==TNum && r'==TNum
                       then TNum
                       else error "Type Mismatch in +"
typeof (Minus l r) = let l' = (typeof l)
                         r' = (typeof r)
                     in if l'==TNum && r'==TNum
                        then TNum
                        else error "Type Mismatch in -"
typeof (Boolean b) = TBool
typeof (And l r) = if (typeof l) == TBool && (typeof r) == TBool
                   then TBool
                   else error "Type mismatch in &&"
typeof (Leq l r) = if (typeof l) == TNum && (typeof r) == TNum
                   then TBool
                   else error "Type mismatch in <="
typeof (IsZero v) = if (typeof v) == TNum
                    then TBool
                    else error "Type mismatch in IsZero"
typeof (If c t e) = if (typeof c) == TBool
                       && (typeof t)==(typeof e)
                    then (typeof t)
                    else error "Type mismatch in if"

interp :: String -> ABE
interp e = let p=(parseABE e) in
           let t=typeof p in
             if ((t==TBool) || (t==TNum))
             then (eval p)
             else error "This should never happen"
