{-
 - ghc -O2 -threaded --make TestStmIo1000000Batchs.hs
 - ./TestStmIo1000000Batchs +RTS -N8
 -}
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TVar (TVar, newTVar, writeTVar, readTVar, modifyTVar)
import Control.Monad.STM (atomically, STM)
import Control.Monad (forever, replicateM)

-- Main du test
main::IO ()
main = do
        m <- atomically creerUneMemoireTest
        _ <- replicateM 100000 $ forkIO $ batchTest m 1000
        ioTest m
           
-------------------------------------- la mŽmoire ---------------------------------------------------
data MemoireTest = MemoireTest {
       idNonConcurrent :: Integer,
       idConcurrent :: TVar Integer
} 

-- Si on veut faire un show sur une variable transactionnelle, il faut utiliser atomically
getConcurrent :: MemoireTest -> IO Integer
getConcurrent m = do
    c <- atomically $ readTVar (idConcurrent m)
    return c

-- La création d'une variable STM est aussi une opération STM -> usage d'une monade
creerUneMemoireTest :: STM MemoireTest
creerUneMemoireTest = do
    conc <- newTVar 100000
    return MemoireTest {idNonConcurrent = 100000, idConcurrent = conc}

-------------------------------------- le batch ---------------------------------------------------
-- Batch de rŽplication du module front
batchTest :: MemoireTest -> Int -> IO ()
batchTest  m recurrenceEnMillis = forever $ do
        threadDelay (recurrenceEnMillis * 5000) -- pas besoin d'appeler liftIO, on est deja dans IO :-)
        atomically $ decrementerCompteur m
        -- c <- getConcurrent m
        -- putStrLn $ "BATCHBATCHBATCHBATCH :" ++ (show c)
        
decrementerCompteur :: MemoireTest -> STM ()
decrementerCompteur m = modifyTVar (idConcurrent m) (\x -> x - 1) -- ModifyTVar prend une TVar et lui applique une fonction pure

-------------------------------------- le IO ---------------------------------------------------
ioTest :: MemoireTest -> IO ()
ioTest m = loop 
    where loop = do
                    putStrLn $ "0 => ImpressionMemoireTest   |   1 => NouvelleValeurCompteur"
                    c <- getLine
                    case c of
                        "0" -> getConcurrent m >>= (putStrLn . show)
                        "1" -> do
                                    putStrLn $ "Nouvelle valeur pour le compteur ?"
                                    v <- getLine
                                    atomically $ mettreAJourValeurDuCompteur m (read v)         
                        _ -> do 
                                    putStrLn $ "Mauvaise commande"
                    loop -- La j'ai cree loop, une boucle sans argument. Ca n'avait pas de sens de renvoyer m quand elle est STM, il suffit de garder une reference permanente dessus (l'argument de ioTest)
        
mettreAJourValeurDuCompteur :: MemoireTest -> Integer -> STM ()
mettreAJourValeurDuCompteur m v = writeTVar (idConcurrent m) v