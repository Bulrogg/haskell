
nombresPremiers :: [Integer]
nombresPremiers = premiersRec [2..]
	where premiersRec (x:xs) = x : premiersRec [a | a <- xs, mod a x /= 0]