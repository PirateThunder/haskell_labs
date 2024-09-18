import Debug.Trace
import Data.Char

int_to_rim :: Int -> [Char]
int_to_rim 0 = ""
int_to_rim x
    | div x 1000 > 0 = "M" ++ int_to_rim (x - 1000)
    | div x 900 > 0 = "CM" ++ int_to_rim (x - 900)
    | div x 500 > 0 = "D" ++ int_to_rim (x - 500)
    | div x 400 > 0 = "CD" ++ int_to_rim (x - 400)
    | div x 100 > 0 = "C" ++ int_to_rim (x - 100)
    | div x 90 > 0 = "XC" ++ int_to_rim (x - 90)
    | div x 50 > 0 = "L" ++ int_to_rim (x - 50)
    | div x 40 > 0 = "XL" ++ int_to_rim (x - 40)
    | div x 10 > 0 = "X" ++ int_to_rim (x - 10)
    | div x 9 > 0 = "IX" ++ int_to_rim (x - 9)
    | div x 5 > 0 = "V" ++ int_to_rim (x - 5)
    | div x 4 > 0 = "IV" ++ int_to_rim (x - 4)
    | div x 1 > 0 = "I" ++ int_to_rim (x - 1)

oct_to_int :: [Char] -> Int
oct_to_int [] = 0
oct_to_int (x:xs) = (digitToInt x) * (8^(length xs)) + (bin_to_int xs)

bin_to_int :: [Char] -> Int
bin_to_int [] = 0
bin_to_int (x:xs) = (digitToInt x) * (2^(length xs)) + (bin_to_int xs)

calculateNum :: [Char] -> [Char] -> Int
calculateNum x y = oct_to_int x + bin_to_int y

calculateOperation :: [Char] -> [Char] -> [Char]
calculateOperation x y = int_to_rim (calculateNum x y)
