module Main where
  import QuoteLookup (runAndParseNew, printResult)
  import System.Environment (getArgs)

  main :: IO ()
  main = do
    args <- getArgs
    putStrLn $ "Got Args: " ++ (show args)
    case args of
      a1:as -> do
        putStrLn $ "Running QuoteLookup using as input: " ++ a1
        r <- runAndParseNew a1
        printResult r
        putStrLn "Done with QuoteLookup"
      [] -> do
        putStrLn "Please provide a file of transactions"


