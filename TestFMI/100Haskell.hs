import Data.List(group)

------------------------------ Probleme 1 --------------------------------
--Find the last element of a list.
myLast0 :: [a] -> a
myLast0 [x] = x
myLast0 (x:l) = myLast0 l

myLast1 :: [a] -> a
myLast1 = head . reverse

myLast2 :: [a] -> a
myLast2 = foldr1 (flip const)
-- foldr1 :: (a -> a -> a) -> [a] -> a
-- const :: a -> b -> a
-- flip :: (a -> b -> c) -> b -> a -> c

myLast3 :: [a] -> a
myLast3 = foldl1 (curry snd)
-- foldl1 :: (a -> a -> a) -> [a] -> a
-- curry :: ((a, b) -> c) -> a -> b -> c
-- snd :: (a, b) -> b

myLast4 :: [a] -> a
myLast4 [] = error "No end for empty lists!"  
myLast4 x = x !! (length x -1)
-- error :: [Char] -> a
-- length :: [a] -> Int

myLast5 :: [a] -> a
myLast5 = last

probleme1 :: IO ()
probleme1 = do 
	putStrLn "---- Probleme 1 : myLast -----"
	putStrLn $ show $ myLast0 [1, 2, 3, 4]
	putStrLn $ show $ myLast1 ["a", "b", "c"]
	putStrLn $ show $ myLast2 ["aa", "bb", "cc"]
	putStrLn $ show $ myLast3 ["aaa", "bbb", "ccc"]
	putStrLn $ show $ myLast4 ["aaaa", "bbbb", "cccc"]
	putStrLn $ show $ myLast5 ["aaaaa", "bbbbb", "ccccc"]



------------------------------ Probleme 2 --------------------------------
-- Find the last but one element of a list.
myButLast0 :: [a] -> a
myButLast0 l = last (reverse (tail (reverse l)))

myButLast1 :: [a] -> a
myButLast1 = last . reverse . tail . reverse

myButLast2 :: [a] -> a
myButLast2 (a:[_]) = a
myButLast2 (_:l) = myButLast2 l

myButLast3 :: [a] -> a
myButLast3 = last . init
-- init :: [a] -> [a]

myButLast4 :: [a] -> a
myButLast4 l = l !! (length l - 2) 

myButLast5 :: [a] -> a
myButLast5 l =  reverse l !! 1

myButLast6 :: [a] -> a
myButLast6 [] = error "vide"
myButLast6 [_] = error "un seul element"
myButLast6 [a,_] = a
myButLast6 (_:l) = myButLast6 l

myButLast7 :: [a] -> a
myButLast7 = head . tail . reverse

probleme2 :: IO ()
probleme2 = do 
	putStrLn "---- Probleme 2 : myButLast-----"
	putStrLn $ show $ myButLast0 [1, 2, 3, 4]
	putStrLn $ show $ myButLast1 ["a", "b", "c"]
	putStrLn $ show $ myButLast2 ["aa", "bb", "cc"]
	putStrLn $ show $ myButLast3 ["aaa", "bbb", "ccc"]
	putStrLn $ show $ myButLast4 ["aaaa", "bbbb", "cccc"]
	putStrLn $ show $ myButLast5 ["aaaaa", "bbbbb", "ccccc"]
	putStrLn $ show $ myButLast6 ["aaaaaa", "bbbbbb", "cccccc"]
	putStrLn $ show $ myButLast7 ["aaaaaaa", "bbbbbbb", "ccccccc"]


------------------------------ Probleme 3 --------------------------------
-- Find the K'th element of a list. The first element in the list is number 1.
elementAt0 :: [a] -> Int -> a
elementAt0 (x:xs) 1 = x
elementAt0 (_:xs) n = elementAt0 xs (n-1)

elementAt1 :: [a] -> Int -> a
elementAt1 l n = last (take n l)

elementAt2 :: [a] -> Int -> a
elementAt2 l n = head (drop (n-1) l)

elementAt3 :: [a] -> Int -> a
elementAt3 l n = l !! (n-1)


probleme3 :: IO ()
probleme3 = do 
	putStrLn "---- Probleme 3 : elementAt-----"
	putStrLn $ show $ elementAt0 "haskell" 1
	putStrLn $ show $ elementAt1 "haskell" 2
	putStrLn $ show $ elementAt2 "haskell" 3
	putStrLn $ show $ elementAt3 "haskell" 4


------------------------------ Probleme 4 --------------------------------
-- Find the K'th element of a list. The first element in the list is number 1.
myLength0 :: [a] -> Int
myLength0 = myLength0' 0
	where 
			myLength0' acc [] = acc
		  	myLength0' acc (_:xs) = myLength0' (acc + 1) xs 

myLength1 :: [a] -> Int
myLength1 = foldl (\acc _ -> acc + 1) 0 
-- foldl :: (a -> b -> a) -> a -> [b] -> a

myLength2 :: [a] -> Int
myLength2 = foldr (\_ acc -> acc + 1) 0
-- foldr :: (a -> b -> b) -> b -> [a] -> b

myLength3 :: [a] -> Int
myLength3 [] = 0
myLength3 (_:xs) = 1 + (myLength3 xs)

myLength4 :: [a] -> Int
myLength4 = foldr (\_ -> (+1)) 0

myLength5 :: [a] -> Int
myLength5 = foldl (const . (+1)) 0

myLength6 :: [a] -> Int
myLength6 = sum . map (\_ -> 1)

probleme4 :: IO ()
probleme4 = do 
	putStrLn "---- Probleme 4 : myLength-----"
	putStrLn $ show $ myLength0 "haskell"
	putStrLn $ show $ myLength1 "haskell"
	putStrLn $ show $ myLength2 "haskell"
	putStrLn $ show $ myLength3 "haskell"
	putStrLn $ show $ myLength4 "haskell"
	putStrLn $ show $ myLength5 "haskell"
	putStrLn $ show $ myLength6 "haskell"


------------------------------ Probleme 5 --------------------------------
-- Reverse a list.

myReverse0 :: [a] -> [a]
myReverse0 [] = []
myReverse0 (x:xs) = (myReverse0 xs) ++ [x]

myReverse1 :: [a] -> [a]
myReverse1 = foldr (\x acc -> acc ++ [x] ) []  

myReverse2 :: [a] -> [a]
myReverse2 = foldl (\a -> (:a) ) []  
-- (:) :: a -> [a] -> [a]
-- (++) :: [a] -> [a] -> [a]

myReverse3 :: [a] -> [a]
myReverse3 = foldl (flip (:)) []
-- prelude version
-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- flip :: (a -> b -> c) -> b -> a -> c

myReverse4 :: [a] -> [a]
myReverse4 = myReverseAcc []
	where 	
		myReverseAcc acc [] = acc
		myReverseAcc acc (x:xs) = myReverseAcc (x:acc) xs

probleme5 :: IO ()
probleme5 = do 
	putStrLn "---- Probleme 5 : myReverse-----"
	putStrLn $ show $ myReverse0 "coucou comment ca va ?"
	putStrLn $ show $ myReverse1 "1 2 3 4 5 6 7 8 9 0"
	putStrLn $ show $ myReverse2 "1 2 3 4 5 6 7 8 9 0"
	putStrLn $ show $ myReverse3 "1 2 3 4 5 6 7 8 9 0"
	putStrLn $ show $ myReverse4 "version avec un accumulateur proche de la version du prelude en performance"


------------------------------ Probleme 6 --------------------------------
-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

isPalindrome0 :: Eq a => [a] -> Bool
isPalindrome0 l = and $ zipWith (==) l (reverse l)

isPalindrome1 :: Eq a => [a] -> Bool
isPalindrome1 l = not $ or $ zipWith (/=) l (reverse l)
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

isPalindrome2 :: Eq a => [a] -> Bool
isPalindrome2 l = l == (reverse l)

isPalindrome3 :: Eq a => [a] -> Bool
isPalindrome3 [] = True
isPalindrome3 [_] = True
isPalindrome3 l = (head l) == (last l) && (isPalindrome3 $ init $ tail l)

isPalindrome4 :: Eq a => [a] -> Bool
isPalindrome4 l = foldl (\acc (x1, x2) -> if x1 == x2 then acc else False) True (zip l (reverse l))
-- zip :: [a] -> [b] -> [(a, b)]
-- foldl :: (a -> b -> a) -> a -> [b] -> a

probleme6 :: IO ()
probleme6 = do 
	putStrLn "---- Probleme 6 : isPalindrome -----"
	putStrLn $ show $ isPalindrome0 "palindrome0"
	putStrLn $ show $ isPalindrome0 "qwertytrewq"
	putStrLn $ show $ isPalindrome1 "palindrome1"
	putStrLn $ show $ isPalindrome1 "qwertytrewq"
	putStrLn $ show $ isPalindrome2 "palindrome2"
	putStrLn $ show $ isPalindrome2 "qwertytrewq"
	putStrLn $ show $ isPalindrome3 "palindrome3"
	putStrLn $ show $ isPalindrome3 "qwertytrewq"
	putStrLn $ show $ isPalindrome4 "palindrome4"
	putStrLn $ show $ isPalindrome4 "qwertytrewq"


------------------------------ Probleme 7 --------------------------------
-- Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]

myFlatten0 :: NestedList a -> [a]
myFlatten0 = myFlatten0' []
				where
					myFlatten0' acc (Elem a) = a : acc
					myFlatten0' acc (List []) = acc
					myFlatten0' acc (List (x:xs)) = (myFlatten0' [] x) ++ (myFlatten0' [] (List xs)) ++ acc

myFlatten1 :: NestedList a -> [a]
myFlatten1 (Elem a) = [a]
myFlatten1 (List l) = concatMap myFlatten1 l
-- concatMap :: (a -> [b]) -> [a] -> [b]

myFlatten2 :: NestedList a -> [a]
myFlatten2 (Elem a) = [a]
myFlatten2 (List []) = []
myFlatten2 (List (x:xs)) = (myFlatten2 x) ++ (myFlatten2 (List xs))

myFlatten3 :: NestedList a -> [a]
myFlatten3 (Elem a) = [a]
myFlatten3 (List l) = foldr (++) [] $ map myFlatten3 l

probleme7 :: IO ()
probleme7 = do 
	putStrLn "---- Probleme 7 : myFlatten -----"
	putStrLn $ show $ myFlatten0 (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
	putStrLn $ show $ myFlatten1 (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
	putStrLn $ show $ myFlatten2 (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
	putStrLn $ show $ myFlatten3 (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])


------------------------------ Probleme 8 --------------------------------
-- Eliminate consecutive duplicates of list elements.

myCompresse0 :: Eq a => [a] -> [a]
myCompresse0 [] = []
myCompresse0 [x] = [x]
myCompresse0 (x1:x2:xs) = if x1 == x2 then myCompresse0 (x2:xs) else x1:(myCompresse0 (x2:xs))  

myCompresse1 :: Eq a => [a] -> [a]
myCompresse1 = map head . group
-- Data.List.group :: Eq a -> [a] -> [[a]]

myCompresse2 :: Eq a => [a] -> [a]
myCompresse2 [] = []
myCompresse2 (x:xs) = x : (dropWhile (== x) (myCompresse2 xs) )
-- dropWhile :: (a -> Bool) -> [a] -> [a]

myCompresse3 :: Eq a => [a] -> [a]
myCompresse3 l = foldr (\x a -> if (head a) == x then a else x:a) [last l] l
-- -- foldr :: (a -> b -> b) -> b -> [a] -> b

probleme8 :: IO ()
probleme8 = do 
	putStrLn "---- Probleme 8 : myCompresse -----"
	putStrLn $ show $ myCompresse0 "a bb ccc dddd     eeeee"
	putStrLn $ show $ myCompresse1 "a bb ccc dddd     eeeee"
	putStrLn $ show $ myCompresse2 "a bb ccc dddd     eeeee"
	putStrLn $ show $ myCompresse3 "a bb ccc dddd     eeeee"

------------------------------ Main ---------------------------------------
main :: IO ()
main = do
	--probleme1
	--probleme2
	--probleme3
	--probleme4
	--probleme5
	--probleme6
	--probleme7
	probleme8