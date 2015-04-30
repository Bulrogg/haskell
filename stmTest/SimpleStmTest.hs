module SimpleStmTest where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

--http://www.haskell.org/haskellwiki/Simple_STM_example 
--newTVar :: a -> STM (TVar a)
--readTVar :: TVar a -> STM a
--atomically :: STM a -> IO a
--forkIO :: IO () -> IO ThreadId
--atomRead :: TVar a -> IO a 
--dispVar :: Show a => TVar a -> IO ()
--appV :: (b -> b) -> TVar b -> IO ()
--pred :: a -> a
--replicateM_ :: Monad m => Int -> m a -> m ()

main :: IO ()
main = do shared <- atomically $ newTVar 0                                  -- initialise une variable partagé à 0 en IO
          before <- atomRead shared                                         -- lit la variable partagé pour en garder une copie
          putStrLn $ "Before: " ++ show before                              -- imprime la valeure before
          _ <- forkIO $ 25 `timesDo` (dispVar shared >> milliSleep 20)      -- thread qui affiche la variable toutes les 20 ms 25 fois d'affiler 
          _ <- forkIO $ 10 `timesDo` (appV ((+) 2) shared >> milliSleep 50) -- thread qui ajoute 2 à la variable toutes les 50 ms 10 fois d'affiler 
          _ <- forkIO $ 20 `timesDo` (appV pred shared >> milliSleep 25)    -- thread qui retire 1 à la variable toutes les 25 ms 20 fois d'affiler
          milliSleep 800                                                    -- on attend 800 ms dans le thread principal
          after <- atomRead shared                                          -- on stocke l'etat de la variable dans after
          putStrLn $ "After: " ++ show after                                -- on affiche after
 where timesDo = replicateM_
       milliSleep = threadDelay . (*) 1000
 
-- permet de lire une TVar en IO 
atomRead :: TVar a -> IO a 
atomRead = atomically . readTVar

-- affiche une TVar
dispVar :: Show a => TVar a -> IO ()
dispVar x = atomRead x >>= print

-- applique une fonction à une TVar
appV :: (b -> b) -> TVar b -> IO ()
appV fn x = atomically $ readTVar x >>= writeTVar x . fn


-- Dans Front, que doit ton mettre en TVar ? La liste des commandes ? La mémoire complète du module Front ?
-- forkIO pour lancer un thread ?