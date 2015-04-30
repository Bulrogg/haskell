module Main 
where
import Test.Hspec

----------------------- Fonction croix

cellule :: Int -> Int -> Int -> Char
cellule cote i j = 
	case (i==j, cote-i+1==j) of
		(True, True) -> 'X'
		(True, False) -> '\\'
		(False, True) -> '/'
		_ -> ' '

croix :: Int -> String
croix cote = unlines $ map (\j -> map (\i -> cellule cote i j) [1..cote]) [1..cote]
               
----------------------- MAin croix

--main = putStrLn $ croix 31

----------------------- Test croix

main = hspec $ do
  describe "fait un x" $ do
    it " un carré de longueur 0" $ do
      croix 0 `shouldBe` ""
    it " un carré de longueur 1" $ do
      croix 1 `shouldBe` "X\n"
    it " un carré de longueur 2" $ do
      croix 2 `shouldBe` "\\/\n/\\\n"
    it " un carré de longueur 3" $ do
      croix 3 `shouldBe` "\\ /\n X \n/ \\\n"