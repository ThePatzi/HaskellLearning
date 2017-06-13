main :: IO ()
main = do
  putStrLn "Hello, whats your name?"

  name <- getLine

  putStrLn ("Hello " ++ name ++ " my friend")
