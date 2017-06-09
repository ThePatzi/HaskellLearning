data Point = Point Float Float  deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 x2) (Point y1 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) dx dy = Circle (Point (x + dx) (y + dy)) r
nudge (Rectangle p1 p2) dx dy = Rectangle (move p1 dx dy) (move p2 dx dy)
  where move (Point x y) dx dy = Point (x + dx) (y + dy)

data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int,
  phoneNumber :: String,
  flavor :: String
} deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector x1 y1 z1) `vplus` (Vector  x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector x y z) `vectMult` t = Vector (x * t) (y * t) (z * t)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector x1 y1 z1) `scalarMult` (Vector  x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

import qualified own_types/Tree as Tree