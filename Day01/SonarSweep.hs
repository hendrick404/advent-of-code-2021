import System.IO (IOMode (..), hGetContents, openFile)

increases :: [Int] -> Int
increases [] = 0
increases [_] = 0
increases (x : y : ys) =
  if y > x
    then increases (y : ys) + 1
    else increases (y : ys)

increaseWindow :: [Int] -> Int
increaseWindow (a : b : c : d : r) =
  if a + b + c < b + c + d
    then increaseWindow (b : c : d : r) + 1
    else increaseWindow (b : c : d : r)
increaseWindow _ = 0

example :: [Int]
example = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

parse :: String -> [Int]
parse str = map read $ lines str

main :: IO ()
main = do
  input <- openFile "input" ReadMode
  content <- hGetContents input
  ints <- return (parse content)
  putStrLn $ "Found " ++ show (increases ints) ++ " increasements in depth"
  putStrLn $ "Found " ++ show (increaseWindow ints) ++ " increasement windows"
  return ()
