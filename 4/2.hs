import List

primes = 2:filter isPrime [3,5..]
isPrime = null . tail . primeFactors
primeFactors n = factor n primes
    where
        factor n (p:ps)
            | p * p > n    = [n]
            | mod n p == 0 = p:factor (div n p) (p:ps)
            | otherwise    = factor n ps

divisor = product . map ((1+) . length) . group . primeFactors
sigma = product . map ((+1).foldl1 (\a x -> x+a*x)).group.primeFactors
aliquot n = sigma n - n

amicablePairs = [(a,b) | a <- [1..], let b = aliquot a, a < b, aliquot b == a]

solution = sum . map addTup.takeWhile under10k $ amicablePairs
    where
        addTup (a,b) = a+b
        under10k (a,b) = a < 10000

problem_21 = do
    putStrLn $ show solution
