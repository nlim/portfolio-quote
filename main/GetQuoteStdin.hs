module Main where
  import QuoteLookup (runRequestStdin, printResult)
  import System.Environment (getArgs)

  main :: IO ()
  main = do
    putStrLn $ "Running QuoteLookup using stdin"
    mr <- runRequestStdin
    maybe (putStrLn "No Result") printResult mr
    putStrLn "Done with QuoteLookup"

