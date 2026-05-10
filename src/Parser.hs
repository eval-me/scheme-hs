-- Parser Combinator Library + Scheme Parser Implementation from Scratch!
module Parser where
import Types

class Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  
newtype Parser a = P (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) s = p s

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P $ \s -> case parse p s of
                         Just (parsed, rest) -> Just (f parsed, rest)
                         Nothing -> Nothing

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = P $ \s -> Just (a, s)
  
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa =  P $ \s -> case parse pf s of
                         Just (fn, rest) ->
                           case parse pa rest of
                             Just (parsed, rest') -> Just (fn parsed, rest')
                             Nothing -> Nothing
                         Nothing -> Nothing

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= fn = P $ \s -> case parse p s of
                         Just (parsed, rest) -> parse (fn parsed) rest
                         Nothing -> Nothing

instance Alternative Parser where
  -- empty :: Parser a
  empty = P $ \_ -> Nothing

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> p' =  P $ \s -> case parse p s of
                         Just (parsed, rest) -> Just (parsed, rest)
                         Nothing -> parse p' s

item :: Parser Char
item = P $ \s -> case s of
  [] -> Nothing
  (x:xs) -> Just (x, xs)

sat :: (Char -> Bool) -> Parser Char
sat p = P $ \s -> case s of
  [] -> Nothing
  (x:xs) -> case p x of
    True -> Just (x, xs)
    False -> Nothing

some :: (Char -> Bool) -> Parser String
some p = do
  h <- sat p 
  t <- some p <|> pure ""
  return (h : t)

many :: (Char -> Bool) -> Parser String
many p = some p <|> pure ""

char :: Char -> Parser Char
char c = sat (==c)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  h <- char x
  t <- string xs
  return (h : t)

ignore :: Char -> Parser ()
ignore c = (many (==c)) *> return ()  

-- Start of the Scheme Parser
whitespace :: Parser ()
whitespace = ignore ' ' <|> ignore '\n' <|> ignore '\t'

symbol :: Parser Expression
symbol = do
  h <- sat isAlphabetic
  t <- many isSymbolChar
  return (Symbol (h : t))

number :: Parser Expression
number = do
  sign    <- string "-" <|> pure ""
  integer <- some isDigit
  decimal <- (do
                 dot <- string "."
                 end <- some isDigit <|> string "0" 
                 return (dot ++ end))
             <|> pure ""
             
  return (Number (read (sign ++ integer ++ decimal) :: Double))

text :: Parser Expression
text = do
  _ <- char '"'
  content <- many (/='\"')
  _ <- char '"'
  return (Text content)

character :: Parser Expression
character = do
  _ <- char '\''
  content <- sat (/='\'')
  _ <- char '\''
  return (Character content)

boolean :: Parser Expression
boolean = do
  val <- string "true" <|> string "false"
  case val of
    "true"  -> return (Boolean True)
    "false" -> return (Boolean False)
    _       -> return (Boolean False)

expressions :: Parser [Expression]
expressions = do
  _ <- whitespace
  h <- expression
  _ <- whitespace
  t <- expressions <|> pure []
  return (h : t)
  
list :: Parser Expression
list = do
  _ <- whitespace
  _ <- char '('
  body <- expressions
  _ <- char ')'
    
  return (List body)
  
nil :: Parser Expression
nil = do
  _ <- string "()" <|> string "nil"
  return Nil

expression :: Parser Expression
expression =
  boolean <|>
  number <|>
  character <|>
  text <|>
  symbol <|>
  nil <|>
  list
