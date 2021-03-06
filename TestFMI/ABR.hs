module ABR (Arbre(Branche, Feuille), inserer, rechercher, construireArbre, aplatir, supprimer, arbreVide) where
--module ABR (inserer, rechercher, construireArbre, aplatir, supprimer, arbreVide) where
-- Pour l'import dans les autres fichiers : import Data.ABR

data Arbre a = Branche a (Arbre a) (Arbre a)
             | Feuille
               deriving Show
 
 
foldArbre f n Feuille = n
foldArbre f n (Branche e d g) = f e (foldArbre f n d) (foldArbre f n g)
 
-- on est tomb� sur une Feuille, donc on n'a rien trouv�
rechercher _ Feuille = False
rechercher e (Branche f g d) | e == f = True
                             | e < f = rechercher e g
                             | e > f = rechercher e d
 
inserer e Feuille = Branche e Feuille Feuille
inserer e (Branche f g d) | e == f = Branche f g d
                          | e < f = Branche f (inserer e g) d
                          | e > f = Branche f g (inserer e d)
 
-- construire un arbre � partir d'une liste
construireArbre :: (Ord a) => [a] -> Arbre a
construireArbre = foldr inserer Feuille
 
aplatir ::  Arbre a -> [a]
aplatir = foldArbre (\e g d -> g ++ [e] ++ d) []
-- version sans foldArbre :
aplatir' Feuille = []
aplatir' (Branche e g d) = aplatir g ++ [e] ++ aplatir d
 
triABR :: (Ord a) => [a] -> [a]
triABR = aplatir . construireArbre
 
 
supprimerPlusGrand (Branche e g Feuille) = (g,e)
supprimerPlusGrand (Branche e g d) = let (d',grand) = supprimerPlusGrand d in
                                     (Branche e g d', grand) 
 
supprimerRacine (Branche _ Feuille Feuille) = Feuille
supprimerRacine (Branche _ g Feuille) = g
supprimerRacine (Branche _ Feuille d) = d
supprimerRacine (Branche _ g d) = Branche e' g' d
    where (g',e') = supprimerPlusGrand g
 
supprimer _ Feuille = Feuille
supprimer e (Branche f g d) | e == f = supprimerRacine (Branche f g d)
                            | e < f = Branche f (supprimer e g) d
                            | e > f = Branche f g (supprimer e d)
 
arbreVide = Feuille