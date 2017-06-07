fib :: (Num a, Eq a) => a -> a
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list!"
maximum' [x] = x
maximum' (x:xs)
  | max > x   = max
  | otherwise = x
  where max = maximum' xs

replica' :: (Num a, Ord a) => a -> b -> [b]
replica' a b
  | a <= 0    = []
  | otherwise = b : replica' (a - 1) b 

take' :: (Num a, Ord a) => a -> [b] -> [b]
take' _ [] = []
take' a _
  | a <= 0    = []
take' a (x: xs) = x : take' (a - 1) xs 

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' a = a : repeat' a

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a, b) : (zip' as bs)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (b:bs)
  | a == b    = True
  | otherwise = elem' a bs 

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [ a | a <- xs, a <= x]
      biggerSorted  = quicksort [ a | a <- xs, a >= x]
  in  smallerSorted ++ [x] ++ biggerSorted