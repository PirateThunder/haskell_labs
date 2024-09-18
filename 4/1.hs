import Char

split :: Char -> String -> [String]
split _ [] = [""]
split x (c:cs) | c == x  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split x cs

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

join :: Char -> [String] -> String
join _ [str] = str
join x (c:cs) = c ++ [x] ++ (join x cs)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let lines = split '\n' contents
    let mx = maximum $ map (\line -> length line) lines
    let result = join '\n' $ map (\line -> (duplicate " " (mx-(length line))) ++ line) lines
 
    writeFile "output.txt" result
