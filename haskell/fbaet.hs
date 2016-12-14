import Text.ParserCombinators.Parsec
import Control.Monad
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Calculator language extended with an environment to hold defined variables

data TFBAE = TNum | TFBAE :->: TFBAE deriving (Show,Eq)

data FBAE = Num Int
          | Plus FBAE FBAE
          | Minus FBAE FBAE
          | Mult FBAE FBAE
          | Div FBAE FBAE
          | Bind String FBAE FBAE
          | Lambda String TFBAE FBAE
          | App FBAE FBAE
          | Id String
          | If FBAE FBAE FBAE
           deriving (Show,Eq)
                    
-- Parser

languageDef =
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
                                    , "Int"]
            , Token.reservedOpNames = [ "+","-","*","/","&&","<=","=",":","->"]
            }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer

expr :: Parser FBAE
expr = buildExpressionParser operators term

operators = [ [Infix (reservedOp "*" >> return (Mult )) AssocLeft,
               Infix (reservedOp "/" >> return (Div )) AssocLeft ]
            , [Infix (reservedOp "+" >> return (Plus )) AssocLeft,
               Infix (reservedOp "-" >> return (Minus )) AssocLeft ]
            ]

numExpr :: Parser FBAE
numExpr = do i <- integer
             return (Num (fromInteger i))

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
       <|> ifExpr
       <|> identExpr
       <|> bindExpr
       <|> lambdaExpr
       <|> appExpr


ty = buildExpressionParser tyoperators tyTerm

tyoperators = [ [Infix (reservedOp "->" >> return (:->: )) AssocLeft ] ]

tyTerm :: Parser TFBAE
tyTerm = do reserved "Nat"
            return TNum
                
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

-- Substitution

subst :: String -> FBAE -> FBAE -> FBAE
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))
subst i v (Div l r) = (Div (subst i v l) (subst i v r))
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
calcs (Mult l r) = let (Num l') = (calcs l)
                       (Num r') = (calcs r)
                   in (Num (l' * r'))
calcs (Div l r) = let (Num l') = (calcs l)
                      (Num r') = (calcs r)
                  in (Num (div l' r'))
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
calc env (Mult l r) = let (Num l') = (calc env l)
                          (Num r') = (calc env r)
                      in (Num (l'*r'))
calc env (Div l r) = let (Num l') = (calc env l)
                         (Num r') = (calc env r)
                      in (Num (div l' r'))
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

