divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f(f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs 

flip' :: (a -> b -> c) -> b -> a -> c
flip' f b a = f a b

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (a:as)
  | f a       = a : filter' f as
  | otherwise = filter' f as

largestDivisible :: (Integral a)  => a
largestDivisible = head (filter' f [100000, 99999..])
  where f x = x `mod` 3829 == 0

collatzSequence :: (Integral a) => a -> [a]
collatzSequence 1 = [1]
collatzSequence a
  | even a  = a:collatzSequence (a `div` 2)
  | odd a   = a:collatzSequence (a * 3 + 1) 

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15)  (map collatzSequence [1..100]))


addThree :: (Num a) => a -> a -> a -> a
addThree a b c = a + b + c

-- is the same as the addThree above
addThree' :: (Num a) => a -> a -> a -> a
addThree' = \a -> \b -> \c -> a + b + c

flipWithLambdas' :: (a -> b -> c) -> b -> a -> c
flipWithLambdas' f = \x y -> f y x

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' a = foldl (\acc x -> if x == a then True else acc) False

mapWithFold' :: (a -> b) -> [a] -> [b]
mapWithFold' f = foldr (\x acc -> f x : acc) []