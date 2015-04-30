#!/usr/bin/env runhaskell

direCoucou :: String
direCoucou = "Coucou " ++ "tout le monde !"


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib ( n - 2)



sieve :: (Integral a) => [a] -> [a]
sieve (n:ns) = n : sieve ns'
  where ns' = filter ((/= 0) . flip rem n) (n:ns)

primes :: (Integral a) => [a]
primes = sieve [2..]


plus1 = plus 1 where plus a b = a + b





fizzbuzz = map (\x -> (show x) ++ "-" ++ concat( map (\(n,s) -> if mod x n == 0 then s else "") conf)) [1..]
	where conf = [(3,"Fizz"),(5,"Buzz")]



fb = map fb3 [1..]
	where fb3 x = case fb2 x of
	      			   "" -> show x
	      			   s -> s
	      fb2 x = concat $ map (fb1 x) conf 
	      fb1 x (n, s) = case mod x n of
	      					0 -> s 
	      					_ -> ""
	      conf = [(3,"Fizz"),(5,"Buzz")]



reverseWords str = unwords $ map reverse $ words str



main :: IO ()
main = do
	--putStrLn direCoucou
	--print $ fib 12
	--putStrLn (show (take 400 primes))
	--putStrLn (show primes)
	--putStrLn (show (take 100 fizzbuzz))
	putStrLn (show (take 100 fb))
	--putStrLn (show (plus1 2))
	--putStrLn (reverseWords "Coucou comment ca va bien ?")