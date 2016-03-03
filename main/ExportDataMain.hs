module Main where
  import HtmlParsing (exportData)
  import System.Environment (getArgs)

  main :: IO ()
  main = do
    args <- getArgs
    case args of
      (a:as) -> exportData a
      []     -> putStrLn "No Symbol File provided"

