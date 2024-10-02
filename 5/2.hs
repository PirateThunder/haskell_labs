module EvalTrees where

-- импортируем модуль для работы с рациональными числами
import Ratio

-- дерево вычислений (или значение - или операция над двумя деревьями)
data EvalTree = Val Integer | Oper Char EvalTree EvalTree deriving Show

-- операции
ops = "+-*/"

-- результат вычисления дерева
eval :: EvalTree -> Rational
eval (Val x) = toRational x
eval (Oper '+' x y) = eval x + eval y
eval (Oper '-' x y) = eval x - eval y
eval (Oper '*' x y) = eval x * eval y
eval (Oper '/' x y)
	| eval y == 0 = 0 -- вообще-то говоря неверно
	| otherwise = eval x / eval y

-- множество разбиений множества на подмножества и дополнения
complements :: [a] -> [([a],[a])]
complements [] = [([],[])]
complements (x:xs) = concat $ map (inject x) (complements xs) where
	inject x (xs,ys) = [(x:xs,ys),(xs,x:ys)]

-- всевозможные деревья для заданного списка чисел
trees :: [Integer] -> [EvalTree]
trees [] = []
trees [x] = [Val x]
trees values = concat
	[allTreesWith op lvs rvs |
		(lvs,rvs) <- complements values,
		op <- ops,
		not (null lvs), not (null rvs)
	] where
	-- всевозможные деревья для операции и зафиксированных чисел слева и справа
	allTreesWith op lvs rvs = [Oper op lt gt | lt <- trees lvs, gt <- trees rvs]

showratio x | denominator x == 1	= show (numerator x)
showratio x | otherwise		= show (numerator x)++ "/"++ show (denominator x)




foo2a target (x:xs)
	| (target == eval x) = (build x ++ "=" ++ (showratio target))
	| otherwise = foo2a target xs

foo2b target = (build tree) ++ "=" ++ (showratio $ eval tree) where tree = int_search target (-125) (Val 0)

int_search [] mx t = t
int_search (x:xs) mx t
	| denominator (eval x) == 1 && (eval x) > mx = int_search xs (eval x) x
	| otherwise = int_search xs mx t

int_check x = x == fromInteger (round x)

build (Val x) = show x
build (Oper '+' x y) = "(" ++ build x ++ "+" ++ build y ++ ")"
build (Oper '-' x y) = "(" ++ build x ++ "-" ++ build y ++ ")"
build (Oper '*' x y) = "(" ++ build x ++ "*" ++ build y ++ ")"
build (Oper '/' x y) = "(" ++ build x ++ "/" ++ build y ++ ")"
