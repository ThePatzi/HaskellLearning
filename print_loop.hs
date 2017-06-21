main :: IO ()
main = do
  putStrLn "Input:"

  input <- getLine

  if null input
      then return()
      else do
        putStrLn $ reverseString input
        main

reverseString :: String -> String
reverseString = foldr (\c cs -> cs ++ [c]) []
