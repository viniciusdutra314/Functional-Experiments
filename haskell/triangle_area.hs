main = do
    line_a <- getLine
    let a = read line_a :: Double
    line_b <- getLine
    let b = read line_b :: Double
    line_c <- getLine
    let c = read line_c :: Double
    let area = triangle_area a b c
    if area<0
        then putStrLn "-"
    else
        putStrLn $ show area

triangle_area a b c
    | not (a + b >= c && b + c >= a && a + c >= b) = -1
    | otherwise = sqrt (s*(s-a)*(s-b)*(s-c))
    where s = ((a + b + c)) / 2