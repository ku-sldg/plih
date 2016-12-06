{-# LANGUAGE GADTs #-}

-- Imports for QuickCheck
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

-- Imports for PLIH
import ParserUtils

--
-- Simple caculator with variables
--
-- Author: Perry Alexander
-- Date: Wed Jul 13 21:20:26 CDT 2016
--
-- Source files for the Binding Arithmetic Expressions extended with
-- Function (FBAE) language from PLIH
--

data TFBAE where
  TNum :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  deriving (Show,Eq)

data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> String -> FBAE -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  deriving (Show,Eq)
                    
-- Parser

expr :: Parser FBAE
expr = buildExpressionParser operators term

operators = [ [ inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft ]
            ]

numExpr :: Parser FBAE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

identExpr :: Parser FBAE
identExpr = do i <- identifier lexer
               return (Id i)

bindExpr :: Parser FBAE
bindExpr = do reserved lexer "bind"
              i <- identifier lexer
              reservedOp lexer "="
              v <- expr
              reserved lexer "in"
              e <- expr
              return (Bind i v e)

ifExpr :: Parser FBAE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)

lambdaExpr :: Parser FBAE
lambdaExpr = do reserved lexer "lambda"
                n <- identifier lexer
                i <- identifier lexer
                fb <- expr
                reserved lexer "in"
                bb <- expr
                return (Lambda n i fb bb)

appExpr :: Parser FBAE
appExpr = do reserved lexer "app"
             f <- expr
             a <- expr
             return (App f a)

term = parens lexer expr
       <|> numExpr
       <|> ifExpr
       <|> identExpr
       <|> bindExpr
       <|> lambdaExpr
       <|> appExpr


-- Parser invocation

parseFBAE = parseString expr

parseFBAEFile = parseFile expr

-- Pretty Printer

pprint :: FBAE -> String
pprint (Num n) = show n
pprint (Id s) = s
pprint (Plus n m) = "(" ++ pprint n ++ "+" ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ "-" ++ pprint m ++ ")"
pprint (Bind n v b) = "(bind " ++ n ++ " = " ++ pprint v ++ " in " ++ pprint b ++ ")"
pprint (Lambda n x fb bb) = "(lambda " ++ n ++ x ++ pprint fb ++ " in " ++ pprint bb ++ ")"
pprint (App l r) = "(app " ++ pprint l ++ " " ++ pprint r ++ ")"

-- Substitution

subst :: String -> FBAE -> FBAE -> FBAE
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Bind i' v' b') = if i==i'
                            then (Bind i' (subst i v v') b')
                            else (Bind i' (subst i v v') (subst i v b'))
subst i v (Id i') = if i==i'
                    then v
                    else (Id i')
       
evals :: FBAE -> FBAE
evals (Num x) = (Num x)
evals (Plus l r) = let (Num l') = (evals l)
                       (Num r') = (evals r)
                   in (Num (l' + r'))
evals (Minus l r) = let (Num l') = (evals l)
                        (Num r') = (evals r)
                    in (Num (l' - r'))
evals (Bind i v b) = (evals (subst i (evals v) b))
evals (Lambda n i fb bb) = (Lambda n i fb bb)
evals (App f a) = let (Lambda n i fb bb) = (evals f)
                      a' = (evals a)
                  in evals (subst i (evals a) b)
evals (If c t e) = let (Num c') = (evals c)
                   in if c'==0 then (evals t) else (evals e)
evals (Id id) = error "Undeclared Variable"

-- Interpreter

type Env = [(String,FBAE)]
type Cont = [(String,TFBAE)]

         
eval :: Env -> FBAE -> FBAE
eval env (Num x) = (Num x)
eval env (Plus l r) = let (Num l') = (eval env l)
                          (Num r') = (eval env r)
                      in (Num (l'+r'))
eval env (Minus l r) = let (Num l') = (eval env l)
                           (Num r') = (eval env r)
                       in (Num (l'-r'))
eval env (Bind i v b) = let v' = eval env v in
                          eval ((i,v'):env) b
eval env (Lambda i t b) = (Lambda i t b)
eval env (App f a) = let (Lambda i t b) = (eval env f)
                         a' = (eval env a)
                     in eval ((i,a'):env) b
eval env (Id id) = case (lookup id env) of
                     Just x -> x
                     Nothing -> error "Varible not found"
eval env (If c t e) = let (Num c') = (eval env c)
                      in if c'==0 then (eval env t) else (eval env e)

interp = (eval []) . parseFBAE

-- Testing (Requires QuickCheck 2)

-- Arbitrary AST Generator

instance Arbitrary FBAE where
  arbitrary =
    sized $ \n -> genFBAE ((rem n 10) + 10) []

genNum =
  do t <- choose (0,100)
     return (Num t)

genPlus n e =
  do s <- genFBAE n e
     t <- genFBAE n e
     return (Plus s t)

genMinus n e =
  do s <- genFBAE n e
     t <- genFBAE n e
     return (Minus s t)

genName =
  do i <- choose ('v','z')
     return [i]

genId e =
  do n <- elements e
     return (Id n)

genBind n e =
  do i <- genName
     v <- genFBAE n e
     b <- genFBAE n (i:e)
     return (Bind i v b)

genLambda n e =
  do i <- genName
     t <- genTFBAE 3
     b <- genFBAE n (i:e)
     return (Lambda i t b)

genApp n e =
  do t1 <- genFBAE n e
     t2 <- genFBAE n e
     return (App t1 t2)
     
genFBAE :: Int -> [String] -> Gen FBAE
genFBAE 0 e =
  do term <- oneof (case e of
                      [] -> [genNum]
                      _ -> [genNum
                           , (genId e)])
     return term
genFBAE n e =
  do term <- oneof [genNum
                   , (genPlus (n-1) e)
                   , (genMinus (n-1) e)
                   , (genLambda (n-1) e)
                   , (genBind (n-1) e)
                   , (genApp (n-1) e)]
     return term

-- Arbitrary AST Generator

instance Arbitrary TFBAE where
  arbitrary =
    sized $ \n -> genTFBAE ((rem n 10) + 10)

genType :: Gen TFBAE
genType = return TNum

genFunType :: Int -> Gen TFBAE
genFunType n = do
  t1 <- genTFBAE (n-1)
  t2 <- genTFBAE (n-1)
  return (t1 :->: t2)
  
genTFBAE :: Int -> Gen TFBAE
genTFBAE 0 = genType
genTFBAE 1 = genType
genTFBAE n = oneof [genType, genFunType (n-1)]

-- Generate Terms from Types

genTy :: Int -> [(String,TFBAE)] -> TFBAE -> Gen FBAE
genTy 0 e TNum = genNum
genTy 0 e (tyd :->: tyr) = genTLambda 0 e tyd tyr
genTy n e TNum = oneof $ [ genNum
                         , genTApp n e TNum] ++ case selectId TNum e of
                                                  [] -> []
                                                  l -> [ elements l ]
                                                  
genTy n e (tyd :->: tyr) = oneof $ [ genTLambda n e tyd tyr
                                   , genTApp n e (tyd :->: tyr) ]
                           ++ case selectId TNum e of
                                [] -> []
                                l -> [ elements l ]
                           

idOfType :: TFBAE -> (String,TFBAE) -> Bool
idOfType ty (id,ty') = ty == ty'

idFromC :: (String,TFBAE) -> FBAE
idFromC (s,_) = (Id s)

selectId :: TFBAE -> [(String,TFBAE)] -> [FBAE]
selectId t c = map idFromC $ filter (idOfType t) c

decFloor n = if n==0 then n else (n-1)

genTLambda :: Int -> [(String,TFBAE)] -> TFBAE -> TFBAE -> Gen FBAE
genTLambda n e tyd tyr = do
  id <- genName
  tyrv <- genTy (decFloor n) ((id,tyd):e) tyr
  return (Lambda id tyd tyrv)

genTApp :: Int -> [(String,TFBAE)] -> TFBAE -> Gen FBAE
genTApp n e tyr = do
  tyd <- genTFBAE n
  lam <- genTy (decFloor n) e (tyd :->: tyr)
  d <- genTy (decFloor n) e tyd
  return (App lam d)

testParser :: Int -> IO ()
testParser n = do
  i <- randomIO
  quickCheckWith stdArgs {maxSuccess=n}
    (\ty -> (let t=unGen (genTy 0 [] ty) (mkStdGen i) 3 in
               (parseFBAE $ pprint t) == t))

testInterp :: Int -> IO ()
testInterp n = do
  i <- randomIO
  quickCheckWith stdArgs {maxSuccess=n}
    (\ty -> (let t=unGen (genTy 0 [] ty) (mkStdGen i) 3 in
               (eval [] t == evals t)))
    
-- randomIO produces a random number in the IO monad

testRandomTFBAE = do
  i <- randomIO
  return $ unGen (genTFBAE 5) (mkStdGen i) 3

testRandomFBAE = do
  i <- randomIO
  ty <- return $ unGen (genTFBAE 5) (mkStdGen i) 3
  t <- return $ unGen (genTy 3 [] ty) (mkStdGen i) 3
  return $ (t,ty)
