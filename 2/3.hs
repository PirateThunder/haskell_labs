import Char

foo3a [x] = ord x <= ord 'T'
foo3a (x:xs) = ord x <= ord 'T' && foo3a xs

foo3b [x] = isDigit x && mod (digitToInt x) 2 == 0
foo3b (x:xs) = isDigit x && mod (digitToInt x) 2 == 0 && foo3b xs
