import Data.List

foo6 :: [(Int, Int)] -> [(Int, Int)]
foo6 = sortBy (\(x1, y1) (x2, y2) -> compare x1 x2)
