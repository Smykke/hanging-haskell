import System.IO
import System.Exit
import Control.Monad (unless)
import Data.Char (toLower)

----- LER CHAVE: Lê a palavra contida no arquivo "chave.txt" e remove a quebra de linha.
--Saída: chave (palavra a ser descoberta) [IO String]
lerChave :: IO String
lerChave = do
	putStrLn "Lendo chave.txt"
	original <- readFile "chave.txt"
	if (length original) == 0
		then do
			putStrLn "Ops. Arquivo vazio!"
			exitWith ExitSuccess
		else return (take ((length original) -1) (map toLower original)) -- Tira o '\n'

----- CODIFICAR: Recebe a palavra a ser descoberta e retorna uma string de underscores ("_")
--com o mesmo tamanho da palavra.
--Entrada: chave [String]
--Saída: cadeia de underscores [String]
codificar :: String -> String
codificar palavra = [ if p == ' ' || p == '-' then p else '_' | p <- palavra]

---- IMPRIMIR: Imprime uma palavra com um espaço entre as letras
--Entrada: palavra [String]
--Saída: imprime o caractere na tela [IO ()]
imprimir :: String -> IO ()
imprimir [] = putChar '\n'
imprimir (x:xs) = do
	putChar x
	putChar ' '
	imprimir xs

---- RESULTADO FINAL: Imprime se o jogador acertou ou não
acertou :: String -> IO()
acertou chave = putStrLn ("Resposta correta - Vitória!\n" ++ chave)

errou :: IO()
errou = putStrLn "Resposta incorreta - Derrota!"

---- ADIVINHAR: Corresponde ao loop principal do jogo. Lê a entrada do usuário,
--compara com a chave e o conduz a uma nova tentativa ou mostra o resultado final.
--Entrada: chava [String], sequência com letras que o usuário já adivinhou +
--underscores [String] e número da tentativa atual [Int]
--Saída: resultado final [IO ()]
adivinhar :: String -> String -> Int -> IO ()
adivinhar chave tentativa count = do
	putStr "\nPrograma: "
	imprimir tentativa
	putStr ("Jogador (" ++ (if count < 8 then show (count) else "ultima chance") ++ "): ")
--where

	palavra <- getLine
 -- verificar se está na púltima tentativa => só pode verificar a palavra inteira e não uma letra
	if (length palavra) == 1 -- Usuário digitou uma letra
		then do
			let letra = toLower(head palavra)
			let nova_tentativa = [ if x == letra then x else y | (x, y) <- zip chave tentativa]
			unless (not(elem letra tentativa)) $ do
				putStrLn "Hm, parece que você já digitou essa letra"
				adivinhar chave tentativa count   -- tem que ser chamado tudo junto no fim ou colocar um exit ao sair (ExitSuccess)
			if chave == nova_tentativa
				then acertou chave
				else if count < 8
					then adivinhar chave nova_tentativa (count+1)
					else errou
		else if (length palavra) == (length chave) -- Usuário tentou adivinhar a palavra
			then if chave == palavra
				then acertou chave
				else errou
			else if (length palavra) == 0 --
				then adivinhar chave tentativa count
				else errou -- Usuário não inseriu uma tentativa válida

main :: IO ()
main = do
	chave <- lerChave
--	putStrLn chave
	adivinhar chave (codificar chave) 1
	putStrLn ("\nElementar, meu caro Watson. Você já exercitou " ++ show(length(chave)) ++ " neurônios hoje.")
