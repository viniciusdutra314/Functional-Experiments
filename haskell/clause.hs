main=do
    la <- getLine
    let a = read la

    lb <- getLine
    let b = read lb
    
    lc <- getLine
    let c = read lc

    putStrLn $ show $ baskara a b c
sinal x
  | x < 0 = -1
  | x == 0 =0
  | otherwise = 1


absolute x
  | x>0 =x
  | otherwise = -x

baskara a b c
    | delta < 0     = []
    | delta == 0    = [x]
    | otherwise = [x', x'']
  where
    delta  = b^2 - 4*a*c
    x  = -b / (2*a)
    x' = (-b + sqrt(delta)) / (2*a)
    x''= (-b - sqrt(delta)) / (2*a)
    
somaPos []=0
somaPos (x:xs)
  | x>0 = x+somaPos xs
  | otherwise = somaPos xs