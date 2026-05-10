module Types where

data Expression
  = Symbol String
  | Number Double
  | Text String
  | Character Char
  | Boolean Bool
  | Lambda [String] Expression
  | List [Expression]
  | Nil

instance Show Expression where
  show (Symbol s) = s
  show (Number n) = show n
  show (Text s) = ('"' : s) ++ "\""
  show (Character c) = show c
  show (Boolean True) = "true"
  show (Boolean False) = "false"
  show (Lambda args e) = "lambda" ++ show args ++ " -> " ++ show e
  show (List []) = ""
  show (List (h:t)) = show h ++ show t
  show Nil = "nil"

instance Eq Expression where
  (Symbol s) == (Symbol s') = s == s'
  (Number n) == (Number n') = n == n'
  (Character c) == (Character c') = c == c'
  (Boolean a) == (Boolean b) = a == b
  (List a) == (List b) = a == b
  _ == _ = False

