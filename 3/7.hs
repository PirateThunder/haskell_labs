import List
p1, p2, p3, p4, p5 :: (Int,Int)->Bool
p1 (x,y) = (mod (x+y) 2) == 0
p2 (x,y) = x > y
p3 (x,y) = (mod x 4) == (mod y 4)
p4 (x,y) = (x + 2*y) < 8
p5 (x,y) = (mod (max x y) 2) == 1
 
foo7 :: [(Int,Int)]->[(Int,Int)->Bool]->[(Int,Int)]
foo7 ix px = [x | x<-ix, all ( $ x) px]

-- foo7 [(1,1),(2,1),(1,1),(1,1),(1,1)] [p1, p2, p3, p4, p5]
