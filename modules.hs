import Data.List
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

ceasarCipherEncode :: Int -> String -> String
ceasarCipherEncode shift msg = 
    let ords    = map ord msg
        shifted = map (+ shift) ords
    in map chr shifted

ceasarCipherDecode :: Int -> String -> String
ceasarCipherDecode shift = ceasarCipherEncode (negate shift)