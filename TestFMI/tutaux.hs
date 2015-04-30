-- déclaration d'une variable
reponse = 42

perimetreCercle r = r * 3.14 * 2

perimetreRectangle larg long = 2*(larg + long)

perimetreCare cote = perimetreRectangle cote cote

a |& b = (a || b) && not (a && b) -- pas de lettre ni de chiffre et ne commence pas par :

{-
Commentaire sur plusieurs lignes
== /= < > <= >= not && || xor

if ... then ... else
-}


enLettre x = case x of
                        0 -> "Zero"
                        1 -> "Un"
                        2 -> "Deux"
                        _ -> "> deux"

ouEstZero x = case x of
                (0,0) -> "Gauche et droite"
                (0,_) -> "Gauche"
                (_,0) -> "Droite"
                _ -> "Nul part"
     
sommePaire t = case t of
                   (x,y) -> x+y
                      
premierNonNul x y = case (x,y) of
                      (0,y) -> y
                      (x,_) -> x
                      
signePremier (x,_)
    | x > 0 = "Positif"
    | x < 0 = "Negatif"
    | otherwise = "Nul" 
    
fst3 (a,_,_) = a
snd3 (_,b,_) = b
thr3 (_,_,c) = c

nombreDeRacines a b c = let delta = b^2 - 4*a*c in
                        if delta > 0 then 2
                        else if delta == 0 then 1
                        else 0
                        
nombreDeRacines' a b c = if delta > 0 then 2
                         else if delta == 0 then 1
                         else 0
    where delta = b^2 - 4*a*c
    
diffSommeProd a b = produit - somme
    where produit = a*b
          somme = a+b
          
myMin x y = if x < y then x else y
myMax x y = if x > y then x else y
myMin4 v w x y = myMin (myMin v w) (myMin x y)
myMax4 v w x y = myMax (myMax v w) (myMax x y)

bornerDans min max x = if x < min 
                       then min 
                       else if x > max 
                            then max
                            else x
                            
triPaire (x,y) = if x < y then (x,y) else (y,x)

addVecteurs (x,y) (xx,yy) = (x+xx,y+yy)


-- Version sans filtrage
premier l = if null l then 0 else head l
 
-- Version avec filtrage
premier' l = case l of
                [] -> 0
                x:_ -> x
 
-- premier '' :
premier'' [] = 0
premier'' (x:_) = x

addition [] = 0
addition [x] = x
addition (x:y:_) = x + y

head' [] = error "Liste vide"
head' (x:_) = x


-- Gestion des erreurs

-- Type Maybe
-- Renvoie l'adresse ip du serveur d'un site web
adresseIP "www.siteduzero.com" = Just "92.243.25.239"
adresseIP "progmod.org" = Just "178.33.42.21"
adresseIP "google.fr" = Just "66.249.92.104"
adresseIP _ = Nothing

--lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
-- lookup 1 [(1,"Un"),(2,"Deux")]      => Just "Un"
-- lookup 1 []      => Nothing


-- Propager les erreurs
divise Nothing _ = Nothing
divise _ Nothing = Nothing
divise _ 0 = Nothing
divise x y = Just (x `div` y)

-- Cas faciles : lorsqu'un argument est "Nothing", on renvoie Nothing pour propager l'erreur
plus Nothing _ = Nothing
plus _ Nothing = Nothing
plus (Just a) (Just b) = Just (a + b)

-- Type Either
plus' (Left erra) _ = Left erra 
plus' _ (Left errb) = Left errb
plus' (Right a) (Right b) = Right (a + b)
 
divise' (Left erra) _ = Left erra
divise' _ (Left errb) = Left errb
divise' (Right _) (Right 0) = Left "Division par 0"
divise' (Right a) (Right b) = Right (a `div` b)


-- Typage
exInt :: Integer
exInt = 42
 
presquePi :: Double
presquePi = 3.14

paireIC :: (Integer, Char)
paireIC = (2, 'a')

liste :: [Integer]
liste = [1,2,3,4,5]
 
message :: String
message = "Hello World !"

numero :: Maybe Integer
numero = Just 123

fctType :: Integer -> [Char] -> (Integer,[Char])
fctType x lc = (x,lc) 

-- Polymorphisme et classes de types
f :: (Num a, Num b) => a -> b -> (a,b) -- contraindre le polymorphisme aux nombres
f x y = (x+1,y+1)

-- fromInteger :: (Num a) => Integer -> a
-- toRational :: (Real a) => a -> Rational
-- toInteger :: (Integral a) => a -> Integer      Integral = nombre entier
-- show :: (Show a) => a -> String     show 42     show [1,2,3]
-- read "42" :: Int       read "[1,2,3]" :: [Int]

-- Récursivité
fac 1 = 1
fac n = n * fac (n-1)

-- Récursion terminale   tail-recursive
facTerm n = facTerAux n 1
facTerAux 1 acc = acc
facTerAux n acc = facTerAux (n-1) (n*acc) 

-- filtrage recursif
longueurListe [] = 0
longueurListe (a:l) = 1 + longueurListe l

somme :: (Num a) => [a] -> a
somme [] = 0
somme (a:l) = a + somme l

minimumListe :: (Ord a) => [a] -> a
minimumListe [x] = x
minimumListe (a:l) = min a (minimumListe l)

genererListe a b
        |a > b = []
        |otherwise = a:genererListe (a + 1) b
                           
fois2 [] = []
fois2 (x:l) = (x*2):fois2 l

renverser [] = []
renverser (x:l) = renverser l ++ [x]

supprimerDeLaListe _ [] = []
supprimerDeLaListe x (a:l) 
        | a==x = supprimerDeLaListe x l 
        | otherwise = a:supprimerDeLaListe x l
        
-- tri par insertion
insertion el [] = [el]
insertion el (x:xs) | x > el    = el:x:xs
                    | otherwise = x:insertion el xs
triInsertion [] = []
triInsertion (x:xs) = insertion x (triInsertion xs)

-- tri par fusion
fusion xs [] = xs
fusion [] ys = ys
fusion (x:xs) (y:ys) | x >= y = y:fusion (x:xs) ys
                     | y >= x = x:fusion xs (y:ys)                     
couper [] = ([],[])
couper (x:[]) = ([x],[])
couper (x:y:l) = let (xs,ys) = couper l in (x:xs,y:ys)

triFusion [] = []
triFusion [x] = [x]
triFusion l = let (xs,ys) = couper l in
              fusion (triFusion xs) (triFusion ys)
              
mapFct f [] = []
mapFct f (x:xs) = f x : mapFct f xs
plus1 x = x + 1
plus2 x = x + 2

-- Fonctions anonymes, opérateurs
fonctionLouche liste = map (\x -> x*x + 2*x + 1) liste
mapListePlus3 l = map (\x -> x + 3) l

-- fonctions sur les listes
d `estUnDiviseur` n = n `mod` d == 0
diviseurs n = filter (`estUnDiviseur` n) [1..n]    -- filter  (a -> Bool) -> [a] -> [a]


chiffres = ['0'..'9']
lettres = ['a'..'z'] ++ ['A'..'Z']
isChiffre car = car `elem` chiffres
isLettre car = car `elem` lettres
lireNombre :: String -> (Integer,String)
lireNombre xs = let (nombre, suite) = span isChiffre xs in -- vaut ("47"," pommes") si l'entrée est "47 pommes"
                let (_,unite) = break isLettre suite in    -- unite vaut "pommes"
                (read nombre, unite)                       -- (47,"pommes")

--takeWhile

longueurFoldr liste = foldr (\ _ longueurReste -> 1 + longueurReste) 0 liste
mapFoldr f liste = foldr (\ x acc -> f x:acc) [] liste
renverserFoldl liste = foldl (\ acc x -> x:acc) [] liste
sommeFoldl liste = foldl (+) 0 liste
produitFoldl liste = foldl (*) 1 liste

-- Composées de fonctions
mapF f = (\xs -> map f xs)
filterF p = (\xs -> filter p xs)
foldrF f k = (\xs -> foldr f k xs)

{-
plusMaybe a b = Just (a+b)     ==   plusMaybe a b = Just $ a + b
-}
fonctionListe = [(+1),(*2),(3-),(2/),abs]
resultatListe = map (\f -> f 5) fonctionListe
resultatListe' = map ($ 5) fonctionListe

maFonctionComposee = (+1) . (*2)
sommeCarresPairs xs = sum . map (^2) . filter even $ xs


-- les types
type Couleur = (Double, Double, Double)
palette :: [Couleur]
palette = [(1,0,0), (0,1,1), (0,1,0)]
inverser :: Couleur -> Couleur
inverser (r,g,b) = (1-r,1-g,1-b)

data Parfum = Chocolat | Vanille | Framboise deriving Show
data Glace = UneBoule Parfum | DeuxBoules Parfum Parfum deriving Show
data Date = Date Int Int Int deriving Show -- AAAA MM JJ
data Client = Client String String Date Double deriving Show

prixParfum :: Parfum -> Double
prixParfum Chocolat = 1.5
prixParfum Vanille = 1.2
prixParfum Framboise = 1.4
prixGlace (UneBoule p) = 0.10 + prixParfum p
prixGlace (DeuxBoules p1 p2) = 0.15 + prixParfum p1 + prixParfum p2

nouveauClient::String -> String -> Client
nouveauClient prenom nom = Client prenom nom (Date 0 0 0) 0 

actualiser::Date -> Double -> Client -> Client
actualiser date prix (Client prenom nom _ somme) = Client prenom nom date (somme + prix) 

sommeClient::Client -> Double
sommeClient (Client _ _ _ s) = s


type Point = (Float, Float)
data Forme = Cercle Point Float | Rectangle Point Point

aire::Forme -> Float
aire (Cercle _ r) = pi * r ^ 2
aire (Rectangle (x1, y1) (x2, y2)) = (abs x1 - x2) * (abs y1 - y2)
  
perimetre::Forme -> Float
perimetre (Cercle _ r) = pi * 2 * r
perimetre (Rectangle (x1,y1) (x2,y2)) = 2 * ((abs x1 - x2) + (abs y1 - y2))

-- Enregistrement
data ClientEnrg = ClientEnrg {
      nom :: String,
      prenom :: String,
      mail :: String,
      premiereCommande :: Date,
      derniereCommande :: Date,
      sommeCommandes :: Float
    } deriving Show
    
{-
let clientEnrg1 = ClientEnrg { 
                        nom = "Millard",
                        prenom = "Frederic",
                        mail = "fmi@octo.com",
                        premiereCommande = Date 2013 3 12,
                        derniereCommande = Date 2013 3 12,
                        sommeCommandes = 3.0
                  }
                  
let clientEnrg2 = ClientEnrg "nom" "prenom" "n@p.com" (Date 2013 3 12) (Date 2013 3 12) 3.0
-}
getNomPrenom (ClientEnrg {nom = nom, prenom = prenom}) = nom ++ " " ++ prenom
getPrenom client = "Hello " ++ prenom client

actualiserClientEnrg :: Date -> Float -> ClientEnrg -> ClientEnrg
actualiserClientEnrg date somme client = client {
                                       derniereCommande = date, 
                                       sommeCommandes = sommeCommandes client + somme
                                     }
                                     
                                     
                                     
-- types paramétrés
data Some a = Some a [a]
someToList :: Some a -> [a]
someToList (Some x xs) = x:xs
add :: a -> Some a -> Some a
add i s = Some i (someToList s)

type Assoc a b = [(a,b)]

data MyList a = Nil | Cons a (MyList a) deriving Show
-- let liste = Cons 1 (Cons 2 Nil)

data Arbre a = Branche a (Arbre a) (Arbre a) | Feuille deriving Show

profondeur Feuille = 0
profondeur (Branche _ a1 a2) = 1 + max (profondeur a1) (profondeur a2)

nbFeuilles Feuille = 1
nbFeuilles (Branche _ a1 a2) = nbFeuilles a1 + nbFeuilles a2

sommeArbre Feuille = 0
sommeArbre (Branche v a1 a2) = v + sommeArbre a1 + sommeArbre a2

allArbre _ Feuille = True
allArbre p (Branche e g d) = p e && allArbre p g && allArbre p d
 
abrValide Feuille = True
abrValide (Branche e g d) = allArbre (< e) g && allArbre (> e) d && abrValide g && abrValide d

rechercher _ Feuille = False
rechercher x (Branche v g d) | x==v = True
                             | x<v = rechercher x g
                             | x>v = rechercher x d 
{-
abr = Branche 10
      (Branche 5 Feuille (Branche 8 (Branche 7 Feuille Feuille) (Branche 9 Feuille Feuille)))
      (Branche 20
       (Branche 15 (Branche 12 Feuille Feuille) (Branche 17 Feuille Feuille))
       (Branche 25 Feuille Feuille))
invalide = Branche 10 Feuille (Branche 3 Feuille Feuille)
 -}
 
foldArbre f n Feuille = n
foldArbre f n (Branche e d g) = f e (foldArbre f n d) (foldArbre f n g)
 
profondeur' = foldArbre (\ _ d g -> 1 + max d g) 0
feuilles' = foldArbre (\ _ d g -> d + g) 1
branches' = foldArbre (\ _ d g -> 1 + d + g) 0
somme' = foldArbre (\ e d g -> e + d + g) 0
 
retourner = foldArbre (\ e d g -> Branche e g d) Feuille
 
inserer e Feuille = Branche e Feuille Feuille
inserer e (Branche f g d) | e == f = Branche f g d
                          | e < f = Branche f (inserer e g) d
                          | e > f = Branche f g (inserer e d)
                          
-- construire un arbre à partir d'une liste
construireArbre :: (Ord a) => [a] -> Arbre a
construireArbre = foldr inserer Feuille
 
aplatir :: Arbre a -> [a]
aplatir = foldArbre (\e g d -> g ++ [e] ++ d) []
-- version sans foldArbre :
aplatir' Feuille = []
aplatir' (Branche e g d) = aplatir' g ++ [e] ++ aplatir' d
 
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
          
-- Expression ---------------                  
data Expr =
            Litt Integer
          | Var String
          | Add Expr Expr
          | Mul Expr Expr
            deriving (Eq{-, Show-})

-- Cette fonction renvoie la valeur d'une variable dans le contexte ctx
valeur ctx var = snd $ head $ filter (\(nom,_) -> nom == var) ctx
 
eval ctx (Add a b) = eval ctx a + eval ctx b
eval ctx (Mul a b) = eval ctx a * eval ctx b
eval ctx (Litt a) = a
eval ctx (Var s) = valeur ctx s 

developper (Add x y) = Add (developper x) (developper y)
developper (Mul x y) = case (developper x, developper y) of
                          (Add a b,y') -> developper (Add (Mul a y') (Mul b y'))
                          (x', Add a b) -> developper (Add (Mul x' a) (Mul x' b))
                          (x',y') -> Mul x' y'
developper e = e
 
-- On évalue l'expression test pour x=5
-- Exemple : cette expression correspond à x^2+7x+1
-- let test = Add (Mul (Var "x") (Var "x")) (Add (Mul (Litt 7) (Var "x")) (Litt 1))
-- let valTest = eval [("x",5)] test
-- let e = developper test



-- ----- Des instances de classes de types --------
data Parfum2 = Chocolat2 | Vanille2 | Framboise2 deriving ({-Eq,-} Show, Read)
    
instance Eq Parfum2 where
    Chocolat2 == Chocolat2 = True
    Vanille2 == Vanille2 = True
    Framboise2 == Framboise2 = True
    _ == _ = False
    
instance Ord Parfum2 where
    _ <= Framboise2 = True -- La Framboise, c'est le mieux
    Chocolat2 <= Chocolat2 = True
    Vanille2 <= Vanille2 = True
    Vanille2 <= Chocolat2 = True
    _ <= _ = False -- Dans le reste des cas, c'est faux
 
 -- Si on peut comparer des a pour l'égalité, on peut comparer des Arbre a pour l'égalité
 -- instance (Eq a) =>  Eq (Arbre a) where a == b = aplatir a == aplatir b
 
{-
instance Show Expr where
    show (Litt a) | a < 0 = "("++ show a ++ ")"
                  | otherwise = show a
    show (Var s) = s
    show (Add a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (Mul a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
-}
    
-- Cette fonction ajoute les parenthèses si nécessaire
addPar k n s | k <= n = s
             | otherwise = "(" ++ s ++ ")"
 
showExpr k (Add a b) = addPar k 0 (showExpr 0 a ++ "+" ++ showExpr 0 b)
showExpr k (Mul a b) = addPar k 1 (showExpr 1 a ++ "*" ++ showExpr 1 b)
showExpr _ (Var s) = s
showExpr _ (Litt a) | a < 0 = "("++ show a ++ ")"
                    | otherwise = show a 
instance Show Expr where
    show e = showExpr 0 e

instance Num Expr where
    (+) = Add
    (*) = Mul
    negate = Mul (Litt (-1))
    fromInteger = Litt
    
{-
x = Var "x"
y = Var "y"
 
exprTest = 3*x^5 + 7*x + 9 + 12 * x * y
-}
    
    
    
-- Création classe de type ---------
class OuiNon a where
    toBool :: a -> Bool

instance OuiNon Bool where
    toBool = id
 
instance OuiNon Int where
    toBool 0 = False
    toBool _ = True
 
instance OuiNon Integer where
    toBool 0 = False
    toBool _ = True
 
instance OuiNon [a] where
    toBool [] = False
    toBool _ = True
 
instance OuiNon (Arbre a) where
    toBool Feuille = False
    toBool _ = True
 
instance OuiNon (Maybe a) where
    toBool Nothing = False
    toBool _ = True
    
    
-- import
 {-
import Data.Map (Map)
import qualified Data.Map as M
 
 
-- Pour compter les lettres, on ajoute au fur et à mesure les lettres trouvées dans notre Map
compterLettres :: String -> Map Char Integer
compterLettres = foldr (M.alter ajouterLettre) M.empty
 
-- Cette fonction modifie la valeur associée à un caractère en fonction de la valeur précédente
ajouterLettre Nothing = Just 1 -- On n'avait pas rencontré le caractère avant
ajouterLettre (Just a) = Just (a+1)
 
nombreLettres :: Map Char Integer -> Char -> Integer
nombreLettres m c = case M.lookup c m of
                      Nothing -> 0 -- la lettre n'est pas présente
                      Just a -> a
-}