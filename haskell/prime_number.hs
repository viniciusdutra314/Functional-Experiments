main=do
    x <- getLine
    let a = read x :: Integer
    y <- getLine
    let b = read y :: Integer
    putStrLn $ show $ maximo $ differences $ filter is_prime [a..b]

maximo::[Integer] -> Integer
maximo [] = 0
maximo xs=maximum xs


differences :: Num a => [a] -> [a]
differences lista = zipWith (-) (tail lista) lista

is_prime::Integer->Bool

is_prime(x)
    | length [n | n <- [1..x], x `mod` n == 0] == 2 = True
    | otherwise = False