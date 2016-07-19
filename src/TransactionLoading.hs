module TransactionLoading (readUntilEnd, readTransactions, readTransactionsFromStdin, readTransactionsFromFile, Transaction, writeAllResults, writeFromTChan) where
  import Control.Monad (replicateM, forever)
  import System.IO
  import Data.Maybe (catMaybes, listToMaybe)
  import Data.List.Split (splitOn)
  import Control.Applicative
  import Control.Monad.STM
  import Control.Concurrent
  import Control.Concurrent.STM.TChan
  import Control.Concurrent.STM.TVar

  maybeRead :: Read a => String -> Maybe a
  maybeRead = fmap fst . listToMaybe . reads

  data Transaction = Transaction { transSymbol :: String, transShares :: Float, transPrice :: Float } deriving (Show)

  toTransaction :: [String] -> Maybe Transaction
  toTransaction (s:n:p:rest) = do
    number <- maybeRead n
    price  <- maybeRead p
    return $ Transaction s number price
  toTransaction _ = Nothing

  readTransactionsFromFile :: FilePath -> IO [(String, (Float, Float))]
  readTransactionsFromFile f = withFile f ReadMode $ readTransactions

  readUntilEnd :: FilePath -> IO [String]
  readUntilEnd f = withFile f ReadMode $ readUntilEnd' []

  readTransactionsFromStdin :: IO [(String, (Float, Float))]
  readTransactionsFromStdin = readTransactions stdin

  readTransactions :: Handle -> IO [(String, (Float, Float))]
  readTransactions h = do
    mts <- readTransactions' h
    return $ fmap (\t -> (transSymbol t, (transShares t, transPrice t))) $ catMaybes mts

  readTransactions' :: Handle -> IO [Maybe Transaction]
  readTransactions' f = do
    lines <- readUntilEnd' [] f
    return  $ fmap (toTransaction . (splitOn ",")) lines

  readUntilEnd' :: [String] -> Handle -> IO [String]
  readUntilEnd' soFar h = do
    eof <- hIsEOF h
    if eof
      then return soFar
      else (hGetLine h) >>= (\line -> readUntilEnd' (line:soFar) h)

  writeFromTChan :: (a -> String) -> FilePath -> (TChan a) -> IO ()
  writeFromTChan toString fp tchan = withFile fp WriteMode $ \h -> do
    hSetBuffering h LineBuffering
    forever $ do
      a <- atomically $ readTChan tchan
      hPutStrLn h (toString a)

  writeAllResults :: FilePath -> [a] -> (a -> IO (Maybe String)) -> IO ()
  writeAllResults fp as f = withFile fp WriteMode $ \h -> do
    hSetBuffering h LineBuffering
    writeAllResults' as f h

  writeAllResults' :: [a] -> (a -> IO (Maybe String)) -> Handle -> IO ()
  writeAllResults' [] _ _ = return ()
  writeAllResults' (a:as) f h = do
    ms <- f a
    let rest = writeAllResults' as f h
    case ms of
      Just s  -> (hPutStrLn h s) >> rest
      Nothing -> rest
