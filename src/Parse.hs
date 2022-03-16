module Parse where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

item :: Parser Char
item = P (\ inp -> case inp of
    []     -> []
    (x:xs) -> [(x,xs)])

-- parse item "" => []
-- parse item "abc" => [('a', "bc")]
-- questo è il parser più basico, che permette di consumare un singolo carattere

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P ( \ inp -> case parse p inp of
        []         -> []
        [(v, out)] -> [(g v, out)])

-- parse (fmap toUpper item) "abc" -> [('A', "bc")]

parseToUpper :: Parser Char -> String -> [(Char, String)]
parseToUpper = parse . fmap toUpper

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P ( \ inp -> [(v, inp)])
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P ( \ inp -> case parse pg inp of
        [] -> []
        [(g, out)] -> parse (fmap g px) out)

-- parse (pure 1) "abc" = [(1, "abc")]

three :: Parser (Char, Char)
three = g <$> item <*> item <*> item where
    g x y z = (x, z)

-- parse three "abcdef" = [(('a', 'c'), "def")]
-- parse three "ab" = []

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser p) -> Parser b
    p >>= f = P ( \ inp -> case parse p inp of
        []         -> []
        [(v, out)] -> parse (f v) out)

three' :: Parser (Char, Char)
three' = do
    x <- item
    item
    z <- item
    return (x, z)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (const [])
    -- (<|>) :: Parser a -> Parser a  -> Parser a 
    p <|> q = P (\ inp -> case parse p inp of
        []         -> parse q inp
        [(v, out)] -> [(v, out)])

-- parse empty "abc" = []
-- parse (item <|> return 'd') "abc" = [('a', "bc")]
-- parse (empty <|> return 'd') "abc" = [('d', "abc")]

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x
        then return x
        else empty

digit :: Parser Char 
digit = sat isDigit 

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char 
alphanum = sat isAlphaNum

char :: Char -> Parser Char 
char c = sat (== c)

-- parse (char 'c') "abc" = []
-- parse (char 'a') "abc" = [('a', "bc")]

string :: String -> Parser String
string [] = return []
string (x:xs) = do
    char x
    string xs
    return (x:xs)

-- parse (string "abc") "abcde" = [("abc", "de")]
-- parse (string "abc") "ab234" = []

-- many :: Alternative f => f a -> f [a]
-- parse (many digit) "123abc" = [("123", "abc")]
-- prende digit finché isDigit
-- NO FAILURE
-- some :: Alternative f => f a -> f [a]
-- come many, solo che se non ne trova neanche uno fallisce

ident :: Parser String
ident = do
    x <- lower
    xs <- many alphanum
    return (x:xs)
-- consuma un identificatore

nat :: Parser Int
nat = do
    xs <- some digit
    return (read xs)

space :: Parser ()
space = do
    many (sat isSpace)
    return ()
-- gonna get rid of spaced

int :: Parser Int 
int = do
    char '-'
    n <- nat
    return (-n)
    <|>
    nat

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v
-- gets a "between spaces token"

identifier :: Parser String
identifier = token ident
-- ottiene un identificatore

natural :: Parser Int
natural = token nat
-- ottiene un numero naturale

integer :: Parser Int 
integer = token int
-- anche negativi

symbol :: String -> Parser String
symbol xs = token (string xs)
-- simboli speciali

nats :: Parser [Int]
nats = do
    symbol "["
    n <- natural
    ns <- many (symbol "," >>= const natural)
    symbol "]"
    return (n:ns)
-- legge una lista di numeri naturali

expr :: Parser Int
expr = do
    t <- term
    do
        symbol "+"
        e <- expr
        return (t + e)
        <|>
        return t

term :: Parser Int
term = do
    f <- factor
    do 
        symbol "*"
        t <- term
        return (f * t)
        <|>
        return f

factor :: Parser Int 
factor =
    natural
    <|> do
        symbol "("
        e <- expr
        symbol ")"
        return e

eval :: String -> Int 
eval xs = case (parse expr xs) of
    [(n, "")]  -> n
    [(_, out)] -> error ("Unused input " ++ out)
    []         -> error "Invalid input"




