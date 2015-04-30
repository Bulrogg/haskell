module InOut where

conversation = do
  putStrLn "Bonjour"
  putStrLn "Au revoir"
  
conversationAvecArg nom = do
  putStrLn $ "Bonjour "++ nom 
  putStrLn "Au revoir"
  
echo = do
  putStrLn "Entrez un mot"
  mot <- getLine
  putStrLn $ "Vous avez dit " ++ mot
  
lireMot = do
  putStrLn "Entrez un mot"
  getLine
 
echo2 = do
  mot <- lireMot
  putStrLn $ "Vous avez dit " ++ mot
 
retourner = do
  mot <- lireMot
  let envers = reverse mot
  putStrLn $ "Dites plutot " ++ envers
  
lireMot2 = do
  putStrLn "Entrez un mot"
  x <- getLine
  putStrLn "Merci !"
  return x
  
motSecret x = do
  putStrLn "Entrez le mot secret"
  m <- getLine
  if x == m
    then putStrLn "Vous avez trouve !"
    else do
      putStrLn "Non, ce n'est pas le mot secret."
      motSecret x
      
plusOuMoins x xmin xmax ncoups = do
  putStrLn $ "Entrez un nombre entre " ++ show xmin ++ " et " ++ show xmax
  y <- readLn
  case compare x y of
    LT -> do
      putStrLn "Plus petit !"
      plusOuMoins x xmin (y-1) (ncoups + 1)
    GT -> do
      putStrLn "Plus grand !"
      plusOuMoins x (y+1) xmax (ncoups + 1)
    EQ -> do
      putStrLn $ "Bravo, vous avez trouve le nombre en " ++ show ncoups ++ " essais"

      
{-
ouiNon :: String -> Maybe Bool
ouiNon s = if s' `elem` oui 
                then Just True
           else if s' `elem` non 
                then Just False
                else Nothing
    where oui = ["y","yes","oui","o"]
          non = ["n","no","non"]
          s' = map toLower s
 
lireValide lire = do
  s <- getLine
  case lire s of
    Nothing -> do
                putStrLn "RŽponse invalide"
                lireValide lire
    Just r -> return r
 
lireOuiNon = lireValide ouiNon
-}