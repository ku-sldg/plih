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
          | Leq FBAE FBAE
          | IsZero FBAE
          | If FBAE FBAE FBAE
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
                                    , "Nat"
                                    , "Bool"
                                    , "true"
                                    , "false" ]
            , Token.reservedOpNames = [ "+","-","*","/","&&","<=","=",":","->"]
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
            , [Infix (reservedOp "&&" >> return (And )) AssocLeft ]
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
         
calc :: FBAE -> Env -> FBAE
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
calc (Bind i v b) env = let v' = calc v env in
                          calc b ((i,v'):env)
calc (Lambda i t b) env = (Lambda i t b)
calc (App f a) env = let (Lambda i t b) = (calc f env)
                         a' = (calc a env)
                     in calc b ((i,a'):env)
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


typeof :: FBAE -> Cont -> TFBAE
typeof (Num x) cont = TNum
typeof (Plus l r) cont = let l' = (typeof l cont)
                             r' = (typeof r cont)
                         in if l'==TNum && r'==TNum
                            then TNum
                            else error "Type Mismatch in +"
typeof (Minus l r) cont = let l' = (typeof l cont)
                              r' = (typeof r cont)
                          in if l'==TNum && r'==TNum then TNum else error "Type Mismatch in -"
typeof (Mult l r) cont = let l' = (typeof l cont)
                             r' = (typeof r cont)
                         in if l'==TNum && r'==TNum
                            then TNum
                            else error "Type Mismatch in *"
typeof (Div l r) cont = let l' = (typeof l cont)
                            r' = (typeof r cont)
                        in if l'==TNum && r'==TNum
                           then TNum
                           else error "Type Mismatch in /"
typeof (Bind i v b) cont = let v' = typeof v cont in
                             typeof b ((i,v'):cont)
typeof (Id id) cont = case (lookup id cont) of
                        Just x -> x
                        Nothing -> error "Varible not found"
typeof (Lambda x t b) cont = let tyB = typeof b ((x,t):cont)
                             in t :->: tyB
typeof (App x y) cont = let tyXd :->: tyXr = typeof x cont
                            tyY = typeof y cont
                        in if tyXd==tyY
                           then tyXr
                           else error "Type mismatch in app"
typeof (Boolean b) cont = TBool
typeof (And l r) cont = if (typeof l cont) == TBool && (typeof r cont) == TBool
                        then TBool
                        else error "Type mismatch in &&"
typeof (Leq l r) cont = if (typeof l cont) == TNum && (typeof r cont) == TNum
                        then TBool
                        else error "Type mismatch in <="
typeof (IsZero v) cont = if (typeof v cont) == TNum
                         then TBool
                         else error "Type mismatch in IsZero"
typeof (If c t e) cont = if (typeof c cont) == TBool
                            && (typeof t cont)==(typeof e cont)
                         then (typeof t cont)
                         else error "Type mismatch in if"


eval :: String -> FBAE
eval e = let p=(parseFBAE e) in
           let t=typeof p [] in
             if (t==TNum)
             then (calc p [])
             else error "This should never happen"
