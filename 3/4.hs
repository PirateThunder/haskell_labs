import Data.List (nub)

foo4 [] = []
foo4 (x:xs) = ((snd x), (fst x)) : foo4 xs

composition fs gs = map (mapping gs) fs where
    mapping :: Eq a => [(a,a)] -> (a,a) -> (a,a)
    mapping [] g = g
    mapping (p:ps) g
        | (fst p) == (snd g) = ((fst g), (snd p))
        | True = mapping ps g

proection1 [] = []
proection1 (p:ps) = nub ((fst p):(proection1 ps))

proection2 [] = []
proection2 (p:ps) = nub ((snd p):(proection2 ps))

decartmult _ [] = []
decartmult xs (y:ys) = (foo xs y) ++ (decartmult xs ys) where

foo [] _ = []
foo (x:xs) y = (x, y) : (foo xs y)
