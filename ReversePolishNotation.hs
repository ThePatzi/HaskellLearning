module ReversePolishNotation (
  solveRPN
) where

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
  where foldingFunction (x : y : xs) "+" = (x + y) : xs
        foldingFunction (x : y : xs) "-" = (x - y) : xs
        foldingFunction (x : y : xs) "*" = (y * x) : xs
        foldingFunction (x : y : xs) "/" = (y / x) : xs
        foldingFunction (x : y : xs) "^" = (y ** x) : xs
        foldingFunction (x : xs) "ln"    = log x:xs
        foldingFunction xs "sum"         = [sum xs]
        foldingFunction xs num           = read num : xs
