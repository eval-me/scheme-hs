module Types where

data Expression
  = Symbol String
  | Number Double
  | Text String
  | Character Char
  | Boolean Bool
  | List [Expression]
  | Nil

instance Show Expression where
  show (Symbol s) = s
  show (Number n) = show n
  show (Text s) = ('"' : s) ++ "\""
  show (Character c) = show c
  show (Boolean True) = "true"
  show (Boolean False) = "false"
  show (List []) = ""
  show (List l) = show l
  show Nil = "nil"

instance Eq Expression where
  (Symbol s) == (Symbol s') = s == s'
  (Number n) == (Number n') = n == n'
  (Character c) == (Character c') = c == c'
  (Boolean a) == (Boolean b) = a == b
  (List a) == (List b) = a == b
  _ == _ = False

isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']

isSign :: Char -> Bool
isSign c = isDigit c || c `elem` "+-"

isNumericChar :: Char -> Bool
isNumericChar c = isDigit c || c == '.'

isAlphabetic :: Char -> Bool
isAlphabetic c = c `elem` ['a'..'z'] || c `elem` ['A'..'Z']

isAlphanumeric :: Char -> Bool
isAlphanumeric c = isAlphabetic c || isDigit c

isSymbolChar :: Char -> Bool
isSymbolChar c = isAlphanumeric c || c `elem` "?!-_+*/<>="
