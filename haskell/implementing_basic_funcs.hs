
main = do
    putStrLn $ 
        show $ 
        pega 2 $ 
        mapa (**3) $ 
        filtro (>0) [-3,-5,1,2,3,4,5]




mapa :: (a->b) -> [a] -> [b]
mapa func [] = []
mapa func (x:xs) = func(x): mapa func xs


filtro :: (a->Bool) -> [a] -> [a]
filtro func []=[]
filtro func (x:xs)
    | func x = x : filtro func xs 
    | otherwise = filtro func xs

pega :: Integer -> [a] -> [a]
pega _ [] =[] -- casos base (acabou a lista)
pega 0 _ = [] --  casos base (n=0)
pega n (x:xs) = x : pega (n-1) xs  