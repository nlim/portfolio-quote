module Spacer where
  import Data.Foldable (maximum)
  import Data.List (transpose)
  import Rainbow

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

  printLinesPretty :: [[(String, Radiant)]] -> IO ()
  printLinesPretty lines = mapM_ action formatted where
    action :: [Chunk String] -> IO ()
    action line = (mapM_ (putChunk) line) >> (putStrLn "")
    formatted :: [[Chunk String]]
    formatted = formatLines2 lines

  formatLines2  :: [[(String, Radiant)]] -> [[Chunk String]]
  formatLines2 = (transpose . formatLines2' . transpose)

  formatLines2' :: [[(String, Radiant)]] -> [[Chunk String]]
  formatLines2' = map formatLine2

  formatLine2 :: [(String, Radiant)] -> [Chunk String]
  formatLine2 values = map (fmap (pad m)) chunks2 where
    chunks2 :: [Chunk String]
    chunks2 = map fst chunks

    chunks :: [(Chunk String, Int)]
    chunks = map (\(s,rad) -> ((fore rad (chunk s)), length s)) values

    m :: Int
    m = maximum $ fmap snd chunks
