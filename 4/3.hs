import List

-- problem 52
has_same_digits a b = (show a) \\ (show b) == []
check n = all (has_same_digits n) (map (n*) [2..6])
problem_52 = head $ filter check [1..]

-- problem 56
digitalSum 0 = 0
digitalSum n =
    let (d,m) = quotRem n 10 in m + digitalSum d

problem_56 =
    maximum [digitalSum (a^b) | a <- [99], b <- [90..99]]
