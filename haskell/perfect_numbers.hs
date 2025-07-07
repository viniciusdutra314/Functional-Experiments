main = do
    line_a <- getLine   
    let a = read line_a :: Integer
    line_b <- getLine
    let b = read line_b :: Integer
    let numeros= [a..b]
    putStrLn $ show $ contador eh_defeituoso numeros
    putStrLn $ show $ contador eh_perfeito numeros
    putStrLn $ show $ contador eh_abundante numeros 


contador :: (a -> Bool) -> [a] -> Integer
contador _ [] = 0
contador predicado (x:xs)
    | predicado x = 1 + contador predicado xs
    | otherwise = contador predicado xs



eh_perfeito :: Integer -> Bool
eh_perfeito x = sum [n | n <- [1..x-1], x `mod` n==0]== x

eh_abundante :: Integer -> Bool
eh_abundante x = sum [n | n <- [1..x-1], x `mod` n==0] > x

eh_defeituoso :: Integer -> Bool
eh_defeituoso x = sum [n | n <- [1..x-1], x `mod` n==0] < x



