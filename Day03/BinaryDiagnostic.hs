import System.IO (IOMode (..), hGetContents, openFile)

data Bit = Zero | One
  deriving (Show)

type Binary = [Bit]

for :: [a] -> (a -> b -> b) -> b -> b
for [] _ = id
for (x : xs) f = (for xs f) . (f x)

binaryToInt :: Binary -> Int
binaryToInt = fst . foldr folder (0, 0)
  where
    folder :: Bit -> (Int, Int) -> (Int, Int)
    folder Zero (v, p) = (v, p + 1)
    folder One (v, p) = (v + 2 ^ p, p + 1)

stringToBinary :: String -> Binary
stringToBinary = map (\c -> if c == '0' then Zero else if c == '1' then One else error $ "Invalid character " ++ show c)

gammaRate :: [(Int, Int)] -> Binary
gammaRate = map (\(x, y) -> if x > y then Zero else One)

epsilonRate :: [(Int, Int)] -> Binary
epsilonRate = map (\(x, y) -> if x > y then One else Zero)

oxygenGeneratorRating :: [String] -> Binary
oxygenGeneratorRating strs = stringToBinary $ (for [0 .. 11] filterF) strs !! 0
  where
    filterF :: Int -> [String] -> [String]
    filterF n strs = let bs = commonBits strs in filter (\str -> filterArg (str !! n) (bs !! n)) strs
    filterArg :: Char -> (Int, Int) -> Bool
    filterArg '0' (zeros, ones) = zeros > ones
    filterArg '1' (zeros, ones) = zeros <= ones
    filterArg c _ = error $ "Invalid character " ++ show c

co2ScrubberRating :: [String] -> Binary
co2ScrubberRating strs = stringToBinary $ (for [0 .. 10] filterF) strs !! 0
  where
    filterF :: Int -> [String] -> [String]
    filterF _ [str] = [str]
    filterF n strs = let bs = commonBits strs in filter (\str -> filterArg (str !! n) (bs !! n)) strs
    filterArg :: Char -> (Int, Int) -> Bool
    filterArg '0' (zeros, ones) = zeros <= ones
    filterArg '1' (zeros, ones) = zeros > ones
    filterArg c _ = error $ "Invalid character " ++ show c

commonBits :: [String] -> [(Int, Int)]
commonBits = foldl folder (replicate 12 (0, 0))
  where
    folder :: [(Int, Int)] -> String -> [(Int, Int)]
    folder b s =
      map
        ( \((zeros, ones), c) -> case c of
            '0' -> (zeros + 1, ones)
            '1' -> (zeros, ones + 1)
            _ -> (zeros, ones)
        )
        (zip b s)

example :: [String]
example = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

main :: IO ()
main = do
  input <- openFile "input" ReadMode
  content <- hGetContents input
  gamma <- return . binaryToInt . gammaRate . commonBits $ lines content
  epsilon <- return . binaryToInt . epsilonRate . commonBits $ lines content
  putStrLn $ "Gamma rate: " ++ show gamma
  putStrLn $ "Epsilon rate " ++ show epsilon
  putStrLn $ "Power consumption: " ++ show (gamma * epsilon)
  oxygen <- return . binaryToInt . oxygenGeneratorRating $ lines content
  co2 <- return . binaryToInt . co2ScrubberRating $ lines content
  putStrLn $ "Oxygen generator rating: " ++ show oxygen
  putStrLn $ "CO2 srubber rating: " ++ show co2
  putStrLn $ "Life support rating : " ++ show (oxygen * co2)
  return ()
