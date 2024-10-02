import List

data Ord x => BST x = Null | Childs x (BST x) (BST x) deriving Show

push x Null = Childs x Null Null
push x (Childs v l r)
	| x <= v = Childs v (push x l) r
	| otherwise = Childs v l (push x r)

init xs = foldl (flip push) Null xs

depth Null = 0
depth (Childs v l r) = max (depth l) (depth r) + 1


with_l (Childs v l r@(Childs rv rl rr))
	| depth rl <= depth rr = (Childs rv (Childs v l rl) rr)
	| otherwise = with_l (Childs v l (with_r r))

with_r (Childs v l@(Childs lv ll lr) r)
	| depth lr <= depth ll = (Childs lv ll (Childs v lr r))
	| otherwise = with_r (Childs v (with_l l) r)

is_balanced (Childs _ l r) = abs (depth l - depth r) <= 1

search x Null = (False, [])
search x (Childs v l r) = search_ x (Childs v l r) [] where
	search_ x Null s = (False, [])
	search_ x (Childs v l r) s
		| x == v = (True, reverse s)
		| x <  v = search_ x l (v:s)
		| x >  v = search_ x r (v:s)

init_balanced [] = Null
init_balanced x = Childs (st !! md) (init_balanced $ take md st) (init_balanced $ drop (md + 1) st) where
	st = sort x
	md = div (length x) 2
