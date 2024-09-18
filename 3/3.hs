import Char

foo3a = filter (\x -> not (isDigit x))
foo3b list = maximum(snd(unzip (filter ((<=8).fst) (zip list [0..]))))
foo3c list = (takeWhile (\x -> x /= minimum list) list, tail $ dropWhile (\x -> x /= maximum list) list)
