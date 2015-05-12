import Test.Hspec
import Data.List (nub, elemIndices, sortBy)
import Data.Function (on)

nombresPremiers :: [Integer]
nombresPremiers = premiersRec [2..]
	where premiersRec (x:xs) = x : premiersRec [a | a <- xs, mod a x /= 0]

-- TODO
-- faire fizzBuzz avec ($)
-- regarder Data.List
-- faire un mélangeur de mots
-- regarder Data.Map 
-- regarder Data.Set
-- regarder Data.Function
-- regarder 
-- regarder 
-- regarder 

------------------ encode / decode ---------------------

type StrCode = [(Char,[Int])]

encodeStr :: String -> StrCode
encodeStr str = map calculTupleCharIndexes listeDeTousLesChars
	where 
		listeDeTousLesChars = nub str
		calculTupleCharIndexes c = (c, elemIndices c str)

decodeStr :: StrCode -> String
decodeStr strCodee =  foldr (:) [] $ map fst listeTuplesDemutualisesOrdonnes
	where 
		listeTuplesDemutualisesOrdonnes = sortBy (compare `on` snd) listeTuplesDemutualises
		listeTuplesDemutualises = concatMap demutualiseTuple strCodee
		demutualiseTuple (c, is) = [(c, i) | i <- is]


--------------------------- Les tests --------------

main = hspec $ do
	describe "la fonction encodeStr" $ do
		it "retourne [] pour \"\"" $ do
			encodeStr "" `shouldBe` []
    	it "retourne [('a',[0])] pour \"a\"" $ do
      		encodeStr "a" `shouldBe` [ ('a',[0]) ]
      	it "retourne [('a',[0,1])] pour \"aa\"" $ do
      		encodeStr "aa" `shouldBe` [ ('a',[0,1]) ]
      	it "retourne [('a',[0,2]), ('b',[1])] pour \"aba\"" $ do
      		encodeStr "aba" `shouldBe` [ ('a',[0,2]), ('b',[1]) ]
	describe "la fonction decodeStr" $ do
		it "retourne \"\" pour []" $ do
			decodeStr [] `shouldBe` ""
    	it "retourne \"a\" pour [('a',[0])]" $ do
      		decodeStr [ ('a',[0]) ] `shouldBe` "a"
      	it "retourne \"aa\" pour [('a',[0,1])]" $ do
      		decodeStr [ ('a',[0,1]) ] `shouldBe` "aa"
      	it "retourne \"aba\" pour [('a',[0,2]), ('b',[1])]" $ do
      		decodeStr [ ('a',[0,2]), ('b',[1]) ] `shouldBe` "aba"
	describe "les fonctions encodeStr et decodeStr sont bien compatible" $ do
    	it "str = decodeStr de encodeStr str" $ do
      		(decodeStr $ encodeStr "coucou c'est moi !") `shouldBe` "coucou c'est moi !"

