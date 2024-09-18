foo2a target [x] = [x == target]
foo2a target (x:xs) = [x == target] ++ foo2a target xs

foo2b target list = map (==target) list

foo2c target = map(\x -> x == target)
