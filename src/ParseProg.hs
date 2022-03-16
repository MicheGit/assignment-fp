module ParseProg where

import Parse
import Control.Applicative

type Name = String

data Expr a =
    EVar Name |
    ENum Int |
    EConstr Int Int |
    EAp (Expr a) (Expr a) |
    ELet IsRec [Def a] (Expr a) |
    ECase (Expr a) [Alter a] |
    ELam [a] (Expr a) deriving Show


type Def a = (a, Expr a)

type Alter a = (Int, [a], Expr a)

data IsRec = NonRecursive | Recursive deriving (Eq, Show)

type Program a = [ScDef a]
type CoreProgram = Program Name
type ScDef a = (Name, [a], Expr a)
type CoreScDef = ScDef Name

parseProg :: Parser (Program Name)
parseProg = do
    p <- parseScDef
    do
        char ';'
        ps <- parseProg
        return (p:ps)
        <|>
        return [p]

parseScDef :: Parser (ScDef Name)
parseScDef = do
    v <- parseVar
    pf <- many parseVar
    char '='
    body <- parseExpr
    return (v, pf, body)


parseExpr :: Parser (Expr Name)
parseExpr = do
    isRec <- do
        symbol "letrec"
        return Recursive
        <|> do
        symbol "let"
        return NonRecursive
    defns <- parseDefns
    symbol "in"
    ELet isRec defns <$> parseExpr
    <|> do
    symbol "case"
    expr <- parseExpr
    symbol "of"
    ECase expr <$> parseAlts
    <|> do
    symbol "\\"
    vars <- many parseVar
    symbol "."
    ELam vars <$> parseExpr
    <|>
    parseExpr1

parseExpr1 :: Parser (Expr Name)
parseExpr1 = parseBinopExpr parseExpr2 [binopRightSide (symbol "|") parseExpr1]

parseExpr2 :: Parser (Expr Name)
parseExpr2 = parseBinopExpr parseExpr3 [binopRightSide (symbol "&") parseExpr2]

parseExpr3 :: Parser (Expr Name)
parseExpr3 = parseBinopExpr parseExpr4 [binopRightSide parseRelop parseExpr4]

parseExpr4 :: Parser (Expr Name)
parseExpr4 = parseBinopExpr parseExpr5 [
    binopRightSide (symbol "+") parseExpr4,
    binopRightSide (symbol "-") parseExpr5
    ]

parseExpr5 :: Parser (Expr Name)
parseExpr5 = parseBinopExpr parseExpr6 [
    binopRightSide (symbol "*") parseExpr5,
    binopRightSide (symbol "/") parseExpr6
    ]

parseExpr6 :: Parser (Expr Name)
parseExpr6 = do
    a <- parseAExpr
    as <- many parseAExpr
    return (foldl EAp a as)

parseBinopExpr :: Parser (Expr Name) -> [Expr Name -> Parser (Expr Name)] -> Parser (Expr Name)
parseBinopExpr leftSide rightSides = do
    e <- leftSide
    foldr ((<|>) . ($ e)) (return e) rightSides

binopRightSide :: Parser Name -> Parser (Expr Name) -> Expr Name -> Parser (Expr Name)
binopRightSide oper parser e = do
    s <- oper
    EAp (EAp (EVar s) e) <$> parser


parseAExpr :: Parser (Expr Name)
parseAExpr = do
    EVar <$> parseVar
    <|> do
    ENum <$> natural
    <|> do
    symbol "Pack"
    symbol "{"
    tag   <- natural
    symbol ","
    arity <- natural
    symbol "}"
    return (EConstr tag arity)
    <|> do
    symbol "("
    e <- parseExpr
    symbol ")"
    return e

parseDef :: Parser (Def Name)
parseDef = do
    var <- parseVar
    symbol "="
    expr <- parseExpr
    return (var, expr)

parseDefns :: Parser [Def Name]
parseDefns = do
    def1 <- parseDef
    defs <- many (do
        symbol ";"
        parseDef)
    return (def1:defs)

parseAlt :: Parser (Alter Name)
parseAlt = do
    symbol "<"
    num <- natural
    symbol ">"
    vars <- many parseVar
    symbol "->"
    expr <- parseExpr
    return (num, vars, expr)

parseAlts :: Parser [Alter Name]
parseAlts = do
    alt1 <- parseAlt
    alts <- many (do
        symbol ";"
        parseAlt)
    return (alt1:alts)


parseVar :: Parser String
parseVar = do
    v <- token var
    if v `elem` coreKeywords
        then empty
        else return v

var :: Parser String
var = do
    x <- letter
    xs <- many (alphanum <|> char '_')
    return (x:xs)

coreKeywords :: [String]
coreKeywords = ["let", "letrec", "in", "case", "of", "Pack"]

parseRelop :: Parser String
parseRelop = foldr ((<|>) . symbol) empty relopsList

relopsList :: [String]
relopsList = ["<", ">", "<=", ">=", "==", "~="]