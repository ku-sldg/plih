{-# LANGUAGE GADTs, KindSignatures, RankNTypes, StandaloneDeriving, FlexibleInstances #-}

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Calculator language extended with type derivation function

data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  deriving (Show,Eq)

data FBAE a where
  Num :: Int -> FBAE Int
  Plus :: FBAE Int -> FBAE Int -> FBAE Int
  Minus :: FBAE Int -> FBAE Int -> FBAE Int
  Mult :: FBAE Int -> FBAE Int -> FBAE Int
  Div :: FBAE Int -> FBAE Int -> FBAE Int
  Bind :: String -> FBAE a -> FBAE a -> FBAE a
  Id :: String -> FBAE a
  Boolean :: Bool -> FBAE Bool
  And :: FBAE Bool -> FBAE Bool -> FBAE Bool
  Leq :: FBAE Int -> FBAE Int -> FBAE Bool
  IsZero :: FBAE Int -> FBAE Bool
  If :: FBAE Bool -> FBAE a -> FBAE a -> FBAE a

deriving instance Show (FBAE a)
                    
-- Parser

-- languageDef =
--   javaStyle { Token.identStart = letter
--             , Token.identLetter = alphaNum
--             , Token.reservedNames = [ "fun"
--                                     , "bind"
--                                     , "in"
--                                     , "if"
--                                     , "then"
--                                     , "else"
--                                     , "isZero"
--                                     , "true"
--                                     , "false" ]
--             , Token.reservedOpNames = [ "+","-","*","/","&&","<=","="]
--             }

-- lexer = Token.makeTokenParser languageDef

-- identifier = Token.identifier lexer
-- reserved = Token.reserved lexer
-- reservedOp = Token.reservedOp lexer
-- parens = Token.parens lexer
-- integer = Token.integer lexer
-- whiteSpace = Token.whiteSpace lexer

-- expr :: a -> Parser (FBAE a)
-- expr = buildExpressionParser operators term

-- operators = [ [Infix (reservedOp "*" >> return (Mult )) AssocLeft,
--                Infix (reservedOp "/" >> return (Div )) AssocLeft ]
--             , [Infix (reservedOp "+" >> return (Plus )) AssocLeft,
--                Infix (reservedOp "-" >> return (Minus )) AssocLeft ]
--             , [Infix (reservedOp "&&" >> return (And )) AssocLeft ]
--             , [Infix (reservedOp "<=" >> return (Leq )) AssocLeft ]
--             , [Prefix (reserved "isZero" >> return (IsZero )) ]
--             ]

-- numExpr :: a -> Parser (FBAE a)
-- numExpr = do i <- integer
--              return (Num (fromInteger i))

-- trueExpr :: a -> Parser (FBAE a)
-- trueExpr = do i <- reserved "true"
--               return (Boolean True)

-- falseExpr :: a -> Parser (FBAE a)
-- falseExpr = do i <- reserved "false"
--                return (Boolean False)

-- ifExpr :: a -> Parser (FBAE a)
-- ifExpr = do reserved "if"
--             c <- expr
--             reserved "then"
--             t <- expr
--             reserved "else"
--             e <- expr
--             return (If c t e)

-- identExpr :: a -> Parser (FBAE a)
-- identExpr = do i <- identifier
--                return (Id i)

-- bindExpr :: a -> Parser (FBAE a)
-- bindExpr = do reserved "bind"
--               i <- identifier
--               reservedOp "="
--               v <- expr
--               reserved "in"
--               e <- expr
--               return (Bind i v e)

-- term = parens expr
--        <|> numExpr
--        <|> trueExpr
--        <|> falseExpr
--        <|> ifExpr
--        <|> identExpr
--        <|> bindExpr

-- -- Parser invocation

-- parseString p str =
--   case parse p "" str of
--     Left e -> error $ show e
--     Right r -> r

-- parseFBAE = parseString expr

-- parseFile p file =
--   do program <- readFile file
--      case parse p "" program of
--        Left e -> print e >> fail "parse error"
--        Right r -> return r

-- parseFBAEFile = parseFile expr

type Env = [(String, FBAE)]

addEnv id v env = ((id,v):env)

type Cont = [(String,TFBAE)]
    
calc :: FBAE a -> Env -> FBAE a
calc (Num x) env = (Num x)
calc (Plus l r) env = let (Num l') = (calc l env)
                          (Num r') = (calc r env)
                      in (Num (l'+r'))
calc (Minus l r) env = let (Num l') = (calc l env)
                           (Num r') = (calc r env)
                       in (Num (l'-r'))
calc (Mult l r) env = let (Num l') = (calc l env)
                          (Num r') = (calc r env)
                      in (Num (l'*r'))
calc (Div l r) env = let (Num l') = (calc l env)
                         (Num r') = (calc r env)
                      in (Num (div l' r'))
--calc (Bind i v b) env = let v'=(calc v env)
--                        in calc b (addEnv i v' env)
calc (Id id) env = case (lookup id env) of
                     Just x -> x
                     Nothing -> error "Varible not found"
calc (Boolean b) env = (Boolean b)
calc (And l r) env = let (Boolean l') = (calc l env)
                         (Boolean r') = (calc r env)
                      in (Boolean (l' && r'))
calc (Leq l r) env = let (Num l') = (calc l env)
                         (Num r') = (calc r env)
                      in (Boolean (l' <= r'))
calc (IsZero v) env = let (Num v') = (calc v env)
                      in (Boolean (v' == 0))
calc (If c t e) env = let (Boolean c') = (calc c env)
                      in if c' then (calc t env) else (calc e env)


-- typeof :: FBAE -> Cont -> TFBAE
-- typeof (Num x) cont = TNum
-- typeof (Plus l r) cont = let l' = (typeof l cont)
--                              r' = (typeof r cont)
--                          in if l'==TNum && r'==TNum
--                             then TNum
--                             else error "Type Mismatch in +"
-- typeof (Minus l r) cont = let l' = (typeof l cont)
--                               r' = (typeof r cont)
--                           in if l'==TNum && r'==TNum
--                              then TNum
--                              else error "Type Mismatch in -"
-- typeof (Mult l r) cont = let l' = (typeof l cont)
--                              r' = (typeof r cont)
--                          in if l'==TNum && r'==TNum
--                             then TNum
--                             else error "Type Mismatch in *"
-- typeof (Div l r) cont = let l' = (typeof l cont)
--                             r' = (typeof r cont)
--                         in if l'==TNum && r'==TNum
--                            then TNum
--                            else error "Type Mismatch in /"
-- typeof (Bind i v b) cont = let v' = typeof v cont in
--                              typeof b ((i,v'):cont)
-- typeof (Id id) cont = case (lookup id cont) of
--                         Just x -> x
--                         Nothing -> error "Varible not found"
-- typeof (Boolean b) cont = TBool
-- typeof (And l r) cont = if (typeof l cont) == TBool && (typeof r cont) == TBool
--                         then TBool
--                         else error "Type mismatch in &&"
-- typeof (Leq l r) cont = if (typeof l cont) == TNum && (typeof r cont) == TNum
--                         then TBool
--                         else error "Type mismatch in <="
-- typeof (IsZero v) cont = if (typeof v cont) == TNum
--                          then TBool
--                          else error "Type mismatch in IsZero"
-- typeof (If c t e) cont = if (typeof c cont) == TBool
--                             && (typeof t cont)==(typeof e cont)
--                          then (typeof t cont)
--                          else error "Type mismatch in if"

-- eval :: String -> FBAE
-- eval e = let p=(parseFBAE e) in
--            let t=typeof p [] in
--              if ((t==TBool) || (t==TNum))
--              then (calc p [])
--              else error "This should never happen"
