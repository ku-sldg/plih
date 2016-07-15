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
  Lambda :: String -> TFBAE -> FBAE -> FBAE
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
                (i,t) <- parens lexer argExpr
                b <- expr
                return (Lambda i t b)

argExpr :: Parser (String,TFBAE)
argExpr = do i <- identifier lexer
             reservedOp lexer ":"
             t <- ty
             return (i,t)

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


ty = buildExpressionParser tyOpTable tyTerm

tyOpTable = [ [inFix "->" (:->:) AssocLeft ] ]

tyTerm :: Parser TFBAE
tyTerm = do reserved lexer "Nat"
            return TNum
                
-- Parser invocation

parseFBAE = parseString expr

parseFBAEFile = parseFile expr

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
       
calcs :: FBAE -> FBAE
calcs (Num x) = (Num x)
calcs (Plus l r) = let (Num l') = (calcs l)
                       (Num r') = (calcs r)
                   in (Num (l' + r'))
calcs (Minus l r) = let (Num l') = (calcs l)
                        (Num r') = (calcs r)
                    in (Num (l' - r'))
calcs (Bind i v b) = (calcs (subst i (calcs v) b))
calcs (Lambda i t b) = (Lambda i t b)
calcs (App f a) = let (Lambda i t b) = (calcs f)
                      a' = (calcs a)
                  in calcs (subst i (calcs a) b)
calcs (If c t e) = let (Num c') = (calcs c)
                   in if c'==0 then (calcs t) else (calcs e)
calcs (Id id) = error "Undeclared Variable"

-- Interpreter

type Env = [(String,FBAE)]
type Cont = [(String,TFBAE)]

         
calc :: Env -> FBAE -> FBAE
calc env (Num x) = (Num x)
calc env (Plus l r) = let (Num l') = (calc env l)
                          (Num r') = (calc env r)
                      in (Num (l'+r'))
calc env (Minus l r) = let (Num l') = (calc env l)
                           (Num r') = (calc env r)
                       in (Num (l'-r'))
calc env (Bind i v b) = let v' = calc env v in
                          calc ((i,v'):env) b
calc env (Lambda i t b) = (Lambda i t b)
calc env (App f a) = let (Lambda i t b) = (calc env f)
                         a' = (calc env a)
                     in calc ((i,a'):env) b
calc env (Id id) = case (lookup id env) of
                     Just x -> x
                     Nothing -> error "Varible not found"
calc env (If c t e) = let (Num c') = (calc env c)
                      in if c'==0 then (calc env t) else (calc env e)

-- Type inference

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
typeof cont (Bind i v b) = let v' = typeof cont v in
                             typeof ((i,v'):cont) b
typeof cont (Id id) = case (lookup id cont) of
                        Just x -> x
                        Nothing -> error "Varible not found"
typeof cont (If c t e) = if (typeof cont c) == TNum
                            && (typeof cont t)==(typeof cont e)
                         then (typeof cont t)
                         else error "Type mismatch in if"
typeof cont (Lambda x t b) = let tyB = typeof ((x,t):cont) b
                             in t :->: tyB
typeof cont (App x y) = let tyXd :->: tyXr = typeof cont x
                            tyY = typeof cont y
                        in if tyXd==tyY
                           then tyXr
                           else error "Type mismatch in app"

typecheck :: FBAE -> FBAE
typecheck e = if (typeof [] e)==TNum then e else error "This should never happen"

eval = (calc []) . typecheck . parseFBAE

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
genTy 0 _ _ = genNum
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

genTLambda :: Int -> [(String,TFBAE)] -> TFBAE -> TFBAE -> Gen FBAE
genTLambda n e tyd tyr = do
  id <- genName
  tyrv <- genTy (n-1) ((id,tyd):e) tyr
  return (Lambda id tyd tyrv)

genTApp :: Int -> [(String,TFBAE)] -> TFBAE -> Gen FBAE
genTApp n e tyr = do
  tyd <- genTFBAE n
  lam <- genTy (n-1) e (tyd :->: tyr)
  d <- genTy (n-1) e tyd
  return (App lam d)

testTyGen :: Int -> IO ()
testTyGen n = quickCheckWith stdArgs {maxSuccess=n}
  (\r -> (\t -> (typeof [] t) == r))

-- randomIO produces a random number

testRandomTFBAE = do
  i <- randomIO
  return $ unGen (genTFBAE 5) (mkStdGen i) 3

testRandomFBAE = do
  i <- randomIO
  ty <- return $ unGen (genTFBAE 5) (mkStdGen i) 3
  t <- return $ unGen (genTy 3 [] ty) (mkStdGen i) 3
  return $ (t,ty)

testTyGen' :: Int -> IO ()
testTyGen' n = do
  i <- randomIO
  quickCheckWith stdArgs {maxSuccess=n}
    (\ty -> (let t=unGen (genTy 3 [] ty) (mkStdGen i) 3 in
               (typeof [] t) == ty))
    
