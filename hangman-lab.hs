import System.IO

lerChave :: IO String
lerChave = do
	putStrLn "Lendo chave.txt"
	original <- readFile "chave.txt"
	return (take ((length original) -1) original) -- Tira o '\n'

codificar :: String -> String
codificar palavra = [ '_' | p <- palavra]

imprimir :: String -> IO ()
imprimir [] = putChar '\n'
imprimir (x:xs) = do
	putChar x
	putChar ' '
	imprimir xs

tamanho :: String -> Int
tamanho palavra = length palavra

adivinhar :: String -> String -> Int -> IO ()
adivinhar chave tentativa count = do
	putStr "\nPrograma: "
	imprimir tentativa
	putStr "Jogador ("
	if count < 8 then putStr $ show count else putStr "ultima chance): "
	putStr "): "
	palavra <- getLine

	if (length palavra) == 1
		then do
			let letra = head palavra
			let nova_tentativa =	[ if x == letra then x else y | (x, y) <- zip chave tentativa]
			if chave == nova_tentativa
				then putStrLn chave ++ "Resposta correta - Vitória!"
				else if count < 8 then adivinhar chave nova_tentativa (count+1)
					else putStrLn "Resposta incorreta - Derrota!"
		else if (length palavra) == (length chave)
			then do
				if chave == palavra
					then putStrLn "Resposta correta - Vitória!"
					else putStrLn "Resposta incorreta - Derrota!"
		else putStrLn "Resposta incorreta - Derrota!"


main :: IO ()
main = do
	chave <- lerChave
--	putStrLn chave
	adivinhar chave (codificar chave) 1
	putStrLn "\nQue pessoa inteligente!"
