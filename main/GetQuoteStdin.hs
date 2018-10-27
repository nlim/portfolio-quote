module Main where
  import QuoteLookup (runAndParseFromStdinNew, printResult)
  import System.Environment (getArgs)

  main :: IO ()
  main = do
    putStrLn $ "Running QuoteLookup using stdin"
    r <- runAndParseFromStdinNew
    printResult r
    putStrLn "Done with QuoteLookup"

