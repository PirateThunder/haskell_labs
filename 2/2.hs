-- 2a
foo2a [x] target
	| x == target = []
	| otherwise = [x]

foo2a (x:xs) target
	| x == target = foo2a xs target
	| otherwise = [x] ++ foo2a xs target



-- 2b
foo2b list = foo2b_ list (0, -1)

foo2b_ [] (index, product) = product

foo2b_ (x:xs) (index, product)
	| mod x 2 == 0 && index > 0 = foo2b_ xs (index+1, if product == -1 then index else  product*index)
	| otherwise = foo2b_ xs (index+1, product)

