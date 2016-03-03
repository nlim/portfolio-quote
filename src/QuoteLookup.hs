{-# LANGUAGE OverloadedStrings #-}
module QuoteLookup where
  import Control.Applicative
  import Network.HTTP.Client
  import Data.Aeson
  import Data.Aeson.Types
  import Data.ByteString.Lazy (ByteString)
  import Data.Text (Text)
  import qualified Data.Vector as V (Vector, toList)
  import Data.List (intersperse, intercalate, foldl')
  import Data.Map
  import Data.Maybe (listToMaybe)
  import Data.Traversable (traverse)
  import Debug.Trace
  import Spacer (printLines)
  import TransactionLoading
  import GHC.Exts (sortWith)

  type TransactionType = (String, (Float, Float))
  type TransactionTotals = Map String (Float, Float)

  data QuoteRaw = QuoteRaw { symbolRaw :: String, prevCloseRaw :: String, priceRaw:: String, dividendShareRaw :: Maybe String, earningShareRaw :: Maybe String} deriving (Show)
  data Quote = Quote { symbol :: String , prevClose :: Float, price :: Float, dividendShare :: Float, earningShare :: Float } deriving (Show)

  toQuote :: QuoteRaw -> Maybe Quote
  toQuote qr = do
    p  <- (maybeRead $ priceRaw qr)
    pc <- (maybeRead $ prevCloseRaw qr)
    let dy = (dividendShareRaw qr) >>= maybeFloat
    let ey = (earningShareRaw qr)  >>= maybeFloat
    return $ Quote (symbolRaw qr) pc p (maybe 0.0 id dy) (maybe 0.0 id ey)

  maybeFloat :: String -> Maybe Float
  maybeFloat = maybeRead

  metric :: (Quote -> (Float, Float) -> Float) -> TransactionTotals -> Quote -> Float
  metric metric' tt q = maybe 0.0 (metric' q) $ Data.Map.lookup (symbol q) tt

  quoteLastValue   = metric (\q (n,p) -> n * (prevClose q))
  quoteValue       = metric (\q (n,p) -> n * (price q))
  quoteDividend    = metric (\q (n,p) -> n * (dividendShare q))
  quoteEarning     = metric (\q (n,p) -> n * (earningShare q))
  quoteCost        = metric (\q (n,p) -> n * p)
  quoteDayDiff     = metric (\q (n,p) -> n * ((price q) - prevClose q))
  quoteDayPercent  = metric (\q (n,p) -> 100.0 * ((price q) / (prevClose q)) - 100.0)
  quoteDiff tt q   = (quoteValue tt q) - (quoteCost tt q)

  portfolioMetric :: (Quote -> Float) -> TransactionTotals -> V.Vector Quote -> Float
  portfolioMetric f tt = sum . (fmap f) . V.toList

  portfolioValue tt      = portfolioMetric (quoteValue tt) tt
  portfolioLastValue tt  = portfolioMetric (quoteLastValue tt) tt
  portfolioDividend tt   = portfolioMetric (quoteDividend tt) tt
  portfolioEarning tt    = portfolioMetric (quoteEarning tt) tt

  portfolioCost :: TransactionTotals -> Float
  portfolioCost tt = sum $ fmap (\(n, p) -> n * p) (Data.Map.elems tt)

  toQuotes :: V.Vector QuoteRaw -> Maybe (V.Vector Quote)
  toQuotes v = traverse (toQuote) v

  type MyResult = V.Vector QuoteRaw

  data StockResult = StockResult { quoteResult :: Quote, todaysPercentChange :: Float, todaysDollarChange :: Float, totalDollarChange :: Float, totalDollarAmount :: Float } deriving Show

  toStockResult :: TransactionTotals -> Quote -> StockResult
  toStockResult tt q =  StockResult q (quoteDayPercent tt q) (quoteDayDiff tt q) (quoteDiff tt q) (quoteValue tt q)

  signedShow :: (Num a, Ord a, Show a) => a -> String
  signedShow a | a > (fromIntegral 0) = '+' : (show a)
  signedShow a = show a

  toRows :: FinalResult -> [StockResult] -> [[String]]
  toRows tt srs = ["Symbol", "Previous Close", "Price", "% of Portfolio", "YearlyDividend", "YearlyEarning", "Today's % Change", "Today's $ Change", "Total $ Change", "Total $ Amount"] : (fmap (toRow tt) (reverse (sortWith (percentPortfolio tt) srs)))

  percentPortfolio :: FinalResult -> StockResult -> Float
  percentPortfolio fr sr = (100.0 * ((totalDollarAmount sr) / (totalValue fr)))

  toRow :: FinalResult -> StockResult -> [String]
  toRow fr sr = [
               symbol $ quoteResult sr,
               show $ prevClose $ quoteResult sr,
               show $ price $ quoteResult sr,
               show $ percentPortfolio fr sr,
               show $ dividendShare $ quoteResult sr,
               show $ earningShare $ quoteResult sr,
               signedShow $ todaysPercentChange sr,
               signedShow $ todaysDollarChange  sr,
               signedShow $ totalDollarChange sr,
               show $ totalDollarAmount sr
             ]

  data FinalResult = FinalResult { totalCostBasis :: Float, yearlyDividend :: Float, yearlyEarning :: Float, totalPercent :: Float, totalChange :: Float, todaysPercent :: Float, todaysChange :: Float, stockResults ::  V.Vector StockResult, totalValue :: Float } deriving (Show)

  runRequest :: FilePath -> IO (Maybe FinalResult)
  runRequest f = do
    m <- newManager defaultManagerSettings
    runAndParse f yqlUrl2 m
  --runRequest f = withManager defaultManagerSettings $ runAndParse f yqlUrl2

  printResult :: FinalResult -> IO ()
  printResult fr = do
    putStrLn $ "Total Cost Basis:      " ++ (show $ totalCostBasis fr)
    putStrLn $ "Total Value:           " ++ (show $ totalValue fr)
    putStrLn $ "Total Percent Change:  " ++ (show $ totalPercent fr)
    putStrLn $ "Total Change:          " ++ (show $ totalChange fr)
    putStrLn $ "Todays Percent Change: " ++ (show $ todaysPercent fr)
    putStrLn $ "Todays Change:         " ++ (show $ todaysChange fr)
    putStrLn $ "Dividend Per Year:     " ++ (show $ yearlyDividend fr)
    putStrLn $ "Earning Per Year:      " ++ (show $ yearlyEarning fr)
    putStrLn $ "DY At Cost:            " ++ (show $ 100.0 * ((yearlyDividend fr) / (totalCostBasis fr)))
    printLines $ (toRows fr) $ V.toList $ stockResults fr

  runAndParse :: FilePath -> (TransactionTotals -> Maybe Request) -> Manager -> IO (Maybe FinalResult)
  runAndParse f tmr m = do
    transactions <- readTransactions f
    let portfolio = mkPortfolio transactions
        mr        = tmr portfolio
    maybe (return Nothing) run mr where
      run :: Request -> IO (Maybe FinalResult)
      run r = do
        transactions <- readTransactions f
        let portfolio = mkPortfolio transactions
        response  <- httpLbs r m
        return $ do
         v <- responseToQuotes response
         let pv  = portfolioValue portfolio v
             pc  = portfolioCost portfolio
             plv = portfolioLastValue portfolio v
             pd  = portfolioDividend portfolio v
             pe  = portfolioEarning portfolio v
             percent = (100.0 * (pv / pc)) - 100.0
             change  = pv - pc
             tPercent = (100.0 * (pv / plv)) - 100.0
             tChange  = pv - plv
         return $ FinalResult pc pd pe percent change tPercent tChange (fmap (toStockResult portfolio) v) pv

  symbols :: TransactionTotals -> [String]
  symbols tt = Data.Map.keys tt

  totals :: [TransactionType] -> TransactionTotals
  totals transactions = Data.List.foldl' f (Data.Map.empty) transactions where
   f m t = let (sym, (n1, p1)) = t
           in case (Data.Map.lookup sym m) of
                Just (n, t) -> Data.Map.insert sym (n1 + n, t + p1 * n1) m
                Nothing     -> Data.Map.insert sym (n1    , p1 * n1   ) m

  mkPortfolio :: [TransactionType] -> TransactionTotals
  mkPortfolio transactions = Data.Map.map (\(n, p) -> (n, p / n)) $ totals transactions

  symbolsString :: TransactionTotals -> String
  symbolsString tt = intercalate "%2C" $ fmap (\s -> "%22" ++ s ++ "%22") $ symbols tt

  yqlUrl2 :: TransactionTotals -> Maybe Request
  yqlUrl2 tt = parseUrl $ "http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.quotes%20where%20symbol%20in%20(" ++ (symbolsString tt) ++ ")%0A%09%09&format=json&diagnostics=true&env=http%3A%2F%2Fdatatables.org%2Falltables.env"

  parseObjects :: Array -> Parser (V.Vector Object)
  parseObjects arr = traverse parseJSON arr

  maybeRead :: Read a => String -> Maybe a
  maybeRead = fmap fst . listToMaybe . reads

  parseArray :: Value -> Parser Array
  parseArray v = parseJSON v

  parseQuotes :: Value -> Parser (V.Vector QuoteRaw)
  parseQuotes v = do
    arr     <- parseArray v
    objects <- parseObjects arr
    tuples  <- traverse parseQuote objects
    return tuples


  parseQuote :: Object -> Parser QuoteRaw
  parseQuote v = QuoteRaw <$> v .: "Symbol" <*> v .: "PreviousClose" <*> v .: "LastTradePriceOnly" <*> v .: "DividendShare" <*> v.: "EarningsShare"

  responseToQuotes :: Response ByteString -> Maybe (V.Vector Quote)
  responseToQuotes r = (parseMyResult r) >>= (toQuotes)

  parseMyResult :: Response ByteString -> Maybe MyResult
  parseMyResult r = do result <- decode (responseBody r)
                       flip parseMaybe result $ \obj -> do
                         query   <- obj .: "query"
                         results <- query .: "results"
                         quote   <- results .: "quote"
                         tuples <- parseQuotes quote
                         return tuples

