module Main where
  import HtmlParsing (exportData)
  import System.Environment (getArgs)

  main :: IO ()
  main = do
    args <- getArgs
    case args of
      (symbolFile:outputFile:as) -> exportData symbolFile outputFile
      []     -> putStrLn "Symbol File, follwed by outputFile need to be provided"

