module Spacer where
  import Data.Foldable (maximum)
  import Data.List (transpose)


  --[
  --  [0, 0, 0, 0, 0],     [0, 0, 0, 0, 0]
  --  [1, 2, 6, 2, 1],     [0, 0, 0, 0, 0]
  --  [3, 4, 8, 3, 7]      [0, 0, 0, 0, 0h]
  --]
  --
  --

  fill :: Int -> a -> [a]
  fill n a = take n $ repeat a

  spaces n = fill n ' '

  padAll :: Int -> [String] -> [String]
  padAll n = map $ pad n

  pad :: Int -> String -> String
  pad m s = s ++ (spaces n) where
    n = 3 + (max 0 (m - (length s)))

  printLines :: [[String]] -> IO ()
  printLines lines = mapM_ (putStrLn) formatted' where
    formatted' :: [String]
    formatted' = map (concat) formatted
    formatted :: [[String]]
    formatted = formatLines lines

  formatLines :: [[String]] -> [[String]]
  formatLines = (transpose . formatLines' . transpose)

  formatLines' :: [[String]] -> [[String]]
  formatLines' = map formatLine

  formatLine :: [String] -> [String]
  formatLine xs = map p xs where
    p :: String -> String
    p s = pad m s
    m :: Int
    m = maximum $ map length xs









