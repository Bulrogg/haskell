import Data.Maybe
import Data.Monoid
import Control.Monad(mapM_)

-- fizz peut être représenté comme ça : cond 3 "Fizz"
cond :: Integer -> String -> Integer -> Maybe String
cond d s n = if n `mod` d == 0 then Just s else Nothing

-- mconcat concatene un a un des elements de la liste, sauf les nothing
fb :: Integer -> String
fb n = maybe (show n) id (mconcat [cond 3 "Fizz", cond 5 "Buzz"] n)

main :: IO ()
main = mapM_ print $ map (\n->show n ++ " - " ++ fb n) [1..100]


--data Maybe a = Nothing | Just a
--maybe :: b -> (a -> b) -> Maybe a -> b
--mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
--Data.Monoid.mconcat :: [a] -> a
--id :: a -> a
--print :: Show a => a -> IO ()
--map :: (a -> b) -> [a] -> [b]
--mconcat :: Monoid a => [a] -> a