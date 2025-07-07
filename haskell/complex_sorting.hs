import Data.List (sortBy)
import Data.Ord (comparing)

data Data = Data {
    country   :: String,
    confirmed :: Int,
    deaths    :: Int ,
    recovery  :: Int,
    active    :: Int
} deriving (Show)

split_comma :: String -> [String]
split_comma [] = [""]
split_comma (x:xs)
   | x == ','  = "" : rest
   | otherwise = (x : head rest) : tail rest
 where
   rest = split_comma xs

construct_data_from_raw :: [String] -> Data
construct_data_from_raw [c, con, de, rec, act] = Data c (read con) (read de) (read rec) (read act)
main = do
  -- Parsing CSV to Data
  input <- getLine
  let [n1,n2,n3,n4] = map read $ words input 
  contents <- readFile "dados.csv"
  let rows_strings = lines contents
  let rows_data= map (\x-> construct_data_from_raw (split_comma x)) rows_strings
  -- Enunciado 1)
  print $ sum $ map active $ filter (\x -> (confirmed x)>=n1) rows_data
  -- Enunciado 2)
  let highest_active_countries = take n2 $ reverse $ sortBy (comparing active) rows_data
  let least_confirmed_highest_active_countries = take n3 $ sortBy (comparing confirmed) highest_active_countries
  print $ sum $ map deaths $ least_confirmed_highest_active_countries
  -- Enunciado 3)
  let n4_biggest_confirmed= take n4 $ reverse $ sortBy (comparing confirmed) rows_data
  let sorted_by_name_n4_biggest_confirmed = sortBy (comparing country) n4_biggest_confirmed
  mapM_ (putStrLn . country)  sorted_by_name_n4_biggest_confirmed