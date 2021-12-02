import System.IO (IOMode (..), hGetContents, openFile)

position :: [(String, Int)] -> (Int, Int)
position = foldl movement (0, 0)

movement :: (Int, Int) -> (String, Int) -> (Int, Int)
movement (x, y) (cmd, n)
  | cmd == "forward" = (x + n, y)
  | cmd == "up" = (x, y - n)
  | cmd == "down" = (x, y + n)
  | otherwise = (x, y)

positionAim :: [(String, Int)] -> (Int, Int,Int)
positionAim = foldl movementAim (0,0,0)

movementAim :: (Int, Int,Int) -> (String, Int) -> (Int, Int,Int)
movementAim (x, y, z) (cmd, n)
  | cmd == "forward" = (x + n, y + (n * z), z)
  | cmd == "up" = (x, y, z - n)
  | cmd == "down" = (x, y, z + n)
  | otherwise = (x, y, z)

parse :: String -> [(String, Int)]
parse str = map (\s -> (words s !! 0, read $ words s !! 1)) $ lines str

main :: IO ()
main = do
  input <- openFile "input" ReadMode
  content <- hGetContents input
  commands <- return (parse content)
  let (x, y) = position commands in putStrLn $ "The product of horizontal position and depth is " ++ show (x * y)
  let (x, y, _) = positionAim commands in putStrLn $ "The product of horizontal position and depth after manual steering is " ++ show (x * y)
  return ()
