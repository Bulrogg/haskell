import Data.Function (on)

-------------------- Je suis une liste en compréhension

-- ghci> [x*2 | x <- [1..10], x*2 >= 12]

-- ghci> [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]


-------------------- Tuples

-- ghci> zip [1..] ["apple", "orange", "cherry", "mango"]

rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]


-------------------- Types et classes de types


-------------------- Classes de types 101

-- Eq, Ord, Show, Read, Enum, Bounded, Num

-- ghci> :t read
-- read :: (Read a) => String -> a

-- ghci> read "5" :: Int       5
-- ghci> read "5" :: Float     5.0
-- ghci> (read "5" :: Float) * 4        20.0
-- ghci> read "[1,2,3,4]" :: [Int]      [1,2,3,4]
-- ghci> read "(3, 'a')" :: (Int, Char)   (3, 'a')

-------------------- Syntaxe des fonctions

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


-------------------- Gardes, gardes !

max' :: (Ord a) => a -> a -> a
max' a b
	| a > b = a
	| otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
	| a > b = GT
	| a == b = EQ
	| otherwise = LT

-------------------- Où !?

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
	| bmi <= skinny = "You're underweight, you emo, you!"
	| bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
	| bmi <= fat = "You're fat! Lose some weight, fatty!"
	| otherwise = "You're a whale, congratulations!"
  where 
  	bmi    = weight / height ^ 2
	skinny = 18.5
	normal = 25.0
	fat    = 30.0

-------------------- Let it be

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
	let 
		sideArea = 2 * pi * r * h
		topArea = pi * r ^2
	in 
	sideArea + 2 * topArea

-- ghci> [let square x = x * x in (square 5, square 3, square 2)]

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-------------------- Expressions case

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of 
										[] -> "empty."
										[x] -> "a singleton list."
										xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
	where 
		what [] = "empty."
		what [x] = "a singleton list."
		what xs = "a longer list."

-------------------- Récursivité

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
	| n <= 0    = []
	| otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
	| n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

repeat' :: a -> [a]
repeat' x = x:repeat' x

quicksort :: (Ord a) => [a] -> [a]
quicksort []     = []
quicksort (x:xs) =
	let 
		smallerSorted = quicksort [a | a <- xs, a <= x]
		biggerSorted  = quicksort [a | a <- xs, a > x]
	in 
	smallerSorted ++ [x] ++ biggerSorted

--------------------- Fonctions d’ordre supérieur

------------- Fonctions curryfiées

-- ghci> max 4 5
-- ghci> (max 4) 5

compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- ghci> applyTwice (+3) 10                16
-- ghci> applyTwice (++ " HAHA") "HEY"     "HEY HAHA HAHA"
-- ghci> applyTwice ("HAHA " ++) "HEY"     "HAHA HAHA HEY"
-- ghci> applyTwice (multThree 2 2) 9      144
-- ghci> applyTwice (3:) [1]               [3,3,1]

-- ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]

-- ghci> zipWith' max [6,3,2,1] [7,3,1,5]
-- ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]


--------------------- Maps et filtres

-- ghci> map (+3) [1,5,3,1,6]                         [4,8,6,4,9]
-- ghci> map (++ "!") ["BIFF", "BANG", "POW"]         ["BIFF!","BANG!","POW!"]
-- ghci> map (replicate 3) [3..6]                     [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]

-- ghci> filter (>3) [1,5,3,2,1,6,4,3,2,1]               [5,6,4]
-- ghci> filter (==3) [1,2,3,4,5]                        [3]
-- ghci> filter even [1..10]                             [2,4,6,8,10]
-- ghci> let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]        [[1,2,3],[3,4,5],[2,2]]
-- ghci> filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"        "uagameasadifeent"
-- ghci> filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"      "GAYBALLS"

-- ghci> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))              166650
-- ghci> sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])              166650

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
	| even n = n:chain (n `div` 2)
	| odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
	where isLong xs = length xs > 15

-- ghci> let listOfFuns = map (*) [0..]
-- ghci> (listOfFuns !! 4) 5                    20

------------------------------ Lambdas

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- ghci> zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]

addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

------------------------------ Plie mais ne rompt pas

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
-- sum' = foldl (+) 0
-- pli gauche KO pour les listes infinies

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
-- pli droite OK pour les listes infinies

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
--reverse' = foldl (\acc x -> x : acc) []
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- ghci> scanl (+) 0 [3,5,2,1]            [0,3,8,10,11]
-- ghci> scanr (+) 0 [3,5,2,1]            [11,8,3,1,0]
-- ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]              [3,4,5,5,7,9,9,9]
-- ghci> scanl (flip (:)) [] [3,2,1]      [[],[3],[2,3],[1,2,3]]

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1


------------------- Appliquer des fonctions avec $  (application de fonction) association a droite

-- ghci> :t ($)
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

-- f (g (z x))    ====     f $ g $ z x

-- ghci> map ($ 3) [(4+), (10*), (^2), sqrt]         [7.0,30.0,9.0,1.7320508075688772]

------------------- Composition de fonctions

-- ghci> :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)

-- ghci> map (negate . abs) [5,-3,-6,7,-3,2,-19,24]

-- ghci> map (negate . sum . tail) [[1..5],[3..6],[1..7]]       [-14,-15,-27]

-- sum . replicate 5 . max 6.7 $ 8.9


-- replicate 100 . product . map (*3) . zipWith max [1, 2, 3, 4, 5] $ [4, 5, 6, 7, 8]

-- fn x = ceiling (negate (tan (cos (max 50 x))))
fn = ceiling . negate . tan . cos . max 50   -- style sans point (sans but)

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

--oddSquareSum =
--	let 
--		oddSquares = filter odd $ map (^2) [1..]
--		belowLimit = takeWhile (<10000) oddSquares
--	in 
--	sum belowLimit

------------------------ Modules

-- ghci> :m + Data.List Data.Map Data.Set

-- Data.List / Data.Map / Data.Set / Data.Function

-- import Data.List (nub, sort)

-- import Data.List hiding (nub)

-- import qualified Data.Map         -->  Data.Map.filter

-- import qualified Data.Map as M    --> M.filter

-- import Data.List
-- intersperse / intercalate / transpose / concat / concatMap / and / or / any / all /
-- iterate / splitAt / takeWhile / dropWhile / span / break / sort / group / inits / tails /
-- init / tail / head / last / isInfixOf / isPrefixOf / isSuffixOf / elem / notElem /
-- partition / find / elemIndex / elemIndices / findIndex / zip / zipWith / lines / unlines /
-- words / unwords / nub / delete / \\ / union / intersect / insert

-- ghci> take 10 $ iterate (*2) 1
-- ghci> let (a,b) = splitAt 3 "foobar" in b ++ a "barfoo"
-- ghci> sum . takeWhile (<10000) . map (^3) $ [1..]
-- ghci> map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
-- ghci> sortBy (compare `on` snd) $ map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,18,8,8,8,8,8,8,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
-- ghci> find (\(val,_,_,_) -> val > 1000) stock
-- ghci> [1..10] \\ [2,5,9]         [1,3,4,6,7,8,10]
-- ghci> insert 'g' $ ['a'..'f'] ++ ['h'..'z']

-- ghci> let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]
-- ghci> sortBy (compare `on` length) xs       [[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]

