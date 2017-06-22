import System.IO

lerChave :: IO String
lerChave = do
	putStrLn "Lendo chave.txt"
	chave <- readFile "chave.txt"
	--chave <- take (length(chave)-1) chave
	putStr "Saida: "
	--putStr chave
	return chave

--imprimirPalavra ::
-- imprimirPalavra x:xs = do
	-- putStr x
	-- putStr " "
	-- imprimirPalavra xs

main :: IO()
main = do
	original <- lerChave
	putStr original
	tentativa <- [if i <= length(original) then '-' else ' '| i <- [1..length(original-1)]]
	putStr tentativa
	--putStrLn "ok!"
