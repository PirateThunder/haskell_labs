import Data.Char

coding :: [Char] -> Int -> [Char]
coding [] _ = []    
coding  (x:xs) n
            | isAlpha x =
                if  isLower x 
                    then 
                        chr (ord 'a' + mod (ord x - ord 'a' + n) 26) : coding xs n
                    else  
                        chr (ord 'A' + mod (ord x - ord 'A' + n) 26) : coding xs n         
            | otherwise = x : coding xs n    


decode :: [Char] -> Int -> [Char]
decode x n = coding x (-n)
