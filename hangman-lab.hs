import System.IO

lerChave :: IO ([(Char, Char)])
lerChave = do
	putStrLn "Lendo chave.txt"
	original <- readFile "chave.txt"
	print "Saida: "
	print original
	let chave = codificar2 original
	print chave
	return chave

--imprimirPalavra ::
-- imprimirPalavra x:xs = do
	-- putStr x
	-- putStr " "
	-- imprimirPalavra xs
codificar :: String -> String
-- codificar palavra = [ if i `mod` 2 == 0 then ' ' else '_' | i <- [1..2*(length palavra)]]

codificar2 :: String -> [(Char, Char)]
codificar2 palavra = [ (x, '_') | x <- take ((length palavra) -1) palavra]

imprimir :: [(Char, Char)] -> String
imprimir palavra = [y | (x, y) <- palavra]

tamanho :: String -> Int
tamanho palavra = length palavra

igual :: (Char, Char) -> Bool
igual (a, b)
  | a == b = True
  | otherwise = False

compara :: [(Char, Char)] -> Bool
compara [] = True
compara xs
  | igual (head xs) = compara (tail xs)
  | otherwise = False

adivinhar :: [(Char, Char)] -> IO ()
adivinhar chave = do
	putStr "\n"
	putStrLn (imprimir chave)
	putStrLn "Letra: "
	letra <- getChar
	let tentativa = [ if x == letra then (x, x) else (x, y) | (x, y) <- chave]
	--putStr tentativa
	if compara tentativa then putStrLn "\nAcertou!" else adivinhar tentativa

main :: IO ()
main = do
	-- input <- openFile "chave.txt" ReadMode
	-- chave <- hGetContents input
	chave <- lerChave
	-- putStrLn chave
	adivinhar chave
	putStrLn "cabou!"
	-- hClose input
