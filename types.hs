removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z


equals :: Eq a => a -> a -> Bool
equals a b = a == b

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe x = "Not between 1 and 2"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

tell :: (Show a) => [a] -> String
tell [] = "This is an empty list"
tell [a] = "This is a list with one element: " ++ show a
tell [a, b] = "This is a list with two elements: " ++ show a ++ ", " ++ show b
tell (a:b:_) = "This list is too long"

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

max' :: (Ord a) => a -> a -> a
max' a b 
  | a > b       = a
  | otherwise   = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b       = GT
  | a < b       = LT
  | otherwise      = EQ

calcBMIs :: (RealFloat a) => [(a, a)] -> [a]
calcBMIs xs = [bmi w s | (w, s) <- xs]
  where bmi w s = w / (s ^ 2)

calcBMIs' :: (RealFloat a) => [(a, a)] -> [a]
calcBMIs' xs = [bmi | (w, s) <- xs, let bmi  = w / (s ^ 2), bmi > 30]

test :: (Num a, Ord a) => [a] -> String
test [] = "Empty"
test (x:[]) 
  | x > 10      = "First bigger than 10"
  | otherwise   = "First smaller than 10"