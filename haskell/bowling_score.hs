import Data.List

main = do
  line <- getLine
  let pinos = map read (words line) :: [Int]
  let rodadas = to_rodadas pinos
  let total_score = sum $ pontuações rodadas
  putStrLn $ print_rodadas rodadas ++ " | " ++ show total_score


-- Função auxiliar que divide a lista de pinos em rodadas 
to_rodadas' :: Int -> [Int] -> [[Int]]
to_rodadas' _ [] = []
to_rodadas' 10 xs = [xs]  -- Último rodada pode conter até 3 jogadas
to_rodadas' rodada_counter (10:xs) = [10] : to_rodadas' (rodada_counter + 1) xs  -- Strike
to_rodadas' rodada_counter (a:b:xs) = [a, b] : to_rodadas' (rodada_counter + 1) xs  -- Jogada normal ou spare

-- Wrapper para começar a contagem de rodadas a partir de 1 usando currying
to_rodadas :: [Int] -> [[Int]] 
to_rodadas (x:xs) = to_rodadas' 1 (x:xs)

-- Calcula a pontuação de cada rodada 
pontuações :: [[Int]] -> [Int]
pontuações [] = []
pontuações (x:xs)
  | x == [10] = [10 + sum (take 2 flatten_list)] ++ pontuações xs -- Strike: 10 + próximos 2 pinos
  | soma_pinos == 10 = [10 + sum (take 1 flatten_list)] ++ pontuações xs -- Spare: 10 + próximo pino
  | otherwise = [soma_pinos] ++ pontuações xs -- rodada normal
  where
    soma_pinos = sum x
    flatten_list = concat xs 

-- Função que imprime uma rodada individual com símbolos especiais para strike e spare
-- Os caracteres estão sem espaçamento, eles ganharam espaçamento na próxima função
print_rodada' :: Int -> [Int] -> String
print_rodada' _ [] = ""
print_rodada' recursion_counter (x:[])  
  | x == 10 && recursion_counter == 1 = "X_"  
  | x == 10 = "X"  
  | otherwise = show x  
print_rodada' recursion_counter (x:xs)
  | x + head xs == 10 && x /= 0 = print_rodada' (recursion_counter + 1) xs ++ "/"  -- Spare
  | x == 10 = print_rodada' (recursion_counter + 1) xs ++ "X"  -- Strike
  | otherwise = print_rodada' (recursion_counter + 1) xs ++ show x  -- Jogada normal


-- Imprime as rodadas formatadas como strings legíveis, separados por " | "
print_rodadas :: [[Int]] -> String
print_rodadas rodadas = intercalate " | " $ map (intersperse ' ') $ map (print_rodada' 1) $ map reverse rodadas