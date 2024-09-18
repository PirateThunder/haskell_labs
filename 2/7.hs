import Data.List


main :: String -> String
main s
	| ((dd < 1) || (dd > 31) || (mm > 12) || (mm < 1) || yy > yy1) = error "invalid date"
	| ((dd1 < 1) || (dd1 > 31) || (mm1 > 12) || (mm1 < 1)) = error "invalid date"
	| otherwise = show (yy1 - yy - (fromEnum (mm > mm1))) ++ ":years:"  ++ show(mm1 - mm + (fromEnum (mm > mm1) * 12) - fromEnum (dd > dd1)) ++ ":months:" ++ show (dd1 - dd + (fromEnum (dd > dd1) * 31)) ++ ":days:"
	    where
			dd = read (take 2 s) :: Int
			mm = read (take 2 (drop 3 s)) :: Int 
			yy = read (take 4 (drop 6 s)) :: Int
			dd1 = read (take 2 (drop 11 s)) :: Int
			mm1 = read (take 2 (drop 14 s)) :: Int 
			yy1 = read (take 4 (drop 17 s)) :: Int
