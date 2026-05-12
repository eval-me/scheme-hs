module Main (main) where

import System.IO (hFlush, stdout)
import Parser
import Types

read' :: IO String
read' = putStr "λ> " >> hFlush stdout >> getLine

eval' :: String -> Maybe Expression
eval' s = case parse expression s of
  Just (e, _) -> Just e
  Nothing -> Nothing

print' :: Maybe Expression -> String
print' e = case e of
  Just e' -> show e'
  Nothing -> "Whoops, Parsing failed!"

loop' :: IO ()
loop' = do
  input <- read'
  case input of
    ":q" -> return ()
    _    -> do
            putStrLn (print' (eval' input))
            loop' 

main :: IO ()
main = loop'
