initMatrix :: Int -> Int -> [[Int]]
initMatrix nbLigne nbCol = map (\ col -> [0..nbLigne]) [0 .. nbCol]

--map :: (a -> b) -> [a] -> [b]

--croix :: Int -> [[Int]]
--croix taille = map (\ ll -> map (\ c -> 0) ll) (initMatrix taille taille)

generateLigne i taille = 
	case i of 
		i==taille -> 'X'
		_ -> '0'


croix taille = map (\ numeroDeLaLigne -> [1..taille]) [1..taille]


main :: IO ()
main = do
	putStrLn $ show $ croix 10