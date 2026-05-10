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

-- Start of the Scheme Parser



-- Remember that because we are "altering the state" of the parsed string, we can use recursion to create recursive descent parsing by sequencing parsers. For example, if we have a parser that parses parentheses and the content in them, we don't have to worry about the content in them if we do:
-- _    <- char '('
-- body <- expression 
-- _    <- char ')' 
-- Parentheses body.
-- In general cons cells and lists are done because of this...
