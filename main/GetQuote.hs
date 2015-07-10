module Main where
  import QuoteLookup (runRequest, printResult)
  import System.Environment (getArgs)

  main :: IO ()
  main = do
    args <- getArgs
    putStrLn $ "Got Args: " ++ (show args)
    case args of
      a1:as -> do
        putStrLn $ "Running QuoteLookup using as input: " ++ a1
        mr <- runRequest a1
        maybe (putStrLn "No Result") printResult mr
        putStrLn "Done with QuoteLookup"
      [] -> do
        putStrLn "Please provide a file of transactions"


