import System.IO

lerChave :: IO String
lerChave = do
	putStrLn "Lendo chave.txt"
	chave <- readFile "chave.txt"
	putStr "Saida: "
	putStr chave
	return chave

--imprimirPalavra ::
-- imprimirPalavra x:xs = do
	-- putStr x
	-- putStr " "
	-- imprimirPalavra xs
codificar :: String -> IO()
codificar palavra = do
	let tam = length palavra
	let codificada = [ if i `mod` 2 == 0 then ' ' else '_' | i <- [1..2*tam]]
	putStrLn codificada

	-- if palavra > 1 then lista <- "_" ++ tamanho palavra-1
	-- else "_"

tamanho :: String -> Int
tamanho palavra = length palavra


main :: IO()
main = do
	input <- openFile "chave.txt" ReadMode
	chave <- hGetContents input
	codificar chave
	-- putStrLn chave
	-- let tam = tamanho chave
	-- print tam
	hClose input
