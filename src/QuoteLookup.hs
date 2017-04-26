{-# LANGUAGE OverloadedStrings #-}
module QuoteLookup where
  import Control.Applicative
  import Network.HTTP.Client
  import Data.Aeson
  import Data.Aeson.Types
  import Data.ByteString.Lazy (ByteString)
  import qualified Data.ByteString.Lazy.Char8 as C
  import Data.Text (Text)
  import qualified Data.Vector as V (Vector, toList)
  import Data.List (intersperse, intercalate, foldl')
  import Data.Map
  import Data.Maybe (listToMaybe)
  import Data.Traversable (traverse)
  import Debug.Trace
  import Spacer (printLines, printLinesPretty)
  import TransactionLoading
  import GHC.Exts (sortWith)
  import System.IO (stdin)
  import qualified Rainbow as R

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
  metric metric' tt q = maybe 0.0 (metric' q) $ (Data.Map.lookup (symbol q) tt) <|> (Data.Map.lookup ("BATS:" ++ (symbol q)) tt)

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

  type ToRow a = FinalResult -> StockResult -> [a]

  headers :: [String]
  headers = ["Symbol", "Previous Close", "Price", "% of Portfolio", "YearlyDividend", "YearlyEarning", "Today's % Change", "Today's $ Change", "Total $ Change", "Total $ Amount"]

  toRows3 :: FinalResult -> [StockResult] -> [[(String, R.Radiant)]]
  toRows3 fr srs = (fmap (\s -> (s, R.white)) headers) : (toRows' toRow fr srs)

  toRows' :: (ToRow a) -> FinalResult -> [StockResult] -> [[a]]
  toRows' tr tt srs = (fmap (tr tt) (reverse (sortWith (percentPortfolio tt) srs)))

  percentPortfolio :: FinalResult -> StockResult -> Float
  percentPortfolio fr sr = (100.0 * ((totalDollarAmount sr) / (totalValue fr)))

  mkWhite :: (Num a, Ord a) => (a, a -> String) -> (String, R.Radiant)
  mkWhite (a, f) = (f a, R.white)

  colorize :: (Num a, Ord a) => (a, a -> String) -> (String, R.Radiant)
  colorize (a, f) = (s, computeRad a) where
    s :: String
    s = f a

  printColorized :: (Num a, Ord a) => (a, a -> String) -> IO ()
  printColorized = R.putChunkLn . (\(s, rad) -> R.fore rad (R.chunk s)) . colorize

  computeRad :: (Num a, Ord a) => a -> R.Radiant
  computeRad a = case (compare a (fromIntegral 0)) of
                   LT -> R.red
                   GT -> R.green
                   EQ -> R.white

  toRow :: ToRow (String, R.Radiant)
  toRow fr sr = [
               (symbol $ quoteResult sr, computeRad $ todaysDollarChange sr),
               mkWhite $ (prevClose $ quoteResult sr, show),
               mkWhite $ (price $ quoteResult sr, show),
               mkWhite $ (percentPortfolio fr sr, show),
               mkWhite $ (dividendShare $ quoteResult sr, show),
               mkWhite $ (earningShare $ quoteResult sr, show),
               colorize $ (todaysPercentChange sr, signedShow),
               colorize $ (todaysDollarChange  sr, signedShow),
               colorize $ (totalDollarChange sr, signedShow),
               mkWhite $ (totalDollarAmount sr, show)
             ]


  data FinalResult = FinalResult { totalCostBasis :: Float, yearlyDividend :: Float, yearlyEarning :: Float, totalPercent :: Float, totalChange :: Float, todaysPercent :: Float, todaysChange :: Float, stockResults ::  V.Vector StockResult, totalValue :: Float } deriving (Show)

  runRequest :: FilePath -> IO (Maybe FinalResult)
  runRequest f = do
    m <- newManager defaultManagerSettings
    runAndParse f googleUrl m

  runRequestStdin :: IO (Maybe FinalResult)
  runRequestStdin = do
    m <- newManager defaultManagerSettings
    runAndParseFromStdin googleUrl m

  printResult :: FinalResult -> IO ()
  printResult fr = do
    putStrLn $ "Total Cost Basis:      " ++ (show $ totalCostBasis fr)
    putStrLn $ "Total Value:           " ++ (show $ totalValue fr)
    (putStr    "Total Percent Change:  ") >> (printColorized (totalPercent fr, show))
    (putStr    "Total Change:          ") >> (printColorized (totalChange fr, show))
    (putStr    "Todays Percent Change: ") >> (printColorized (todaysPercent fr, show))
    (putStr    "Todays Change:         ") >> (printColorized (todaysChange fr, show))
    putStrLn $ "Dividend Per Year:     " ++ (show $ yearlyDividend fr)
    putStrLn $ "Earning Per Year:      " ++ (show $ yearlyEarning fr)
    putStrLn $ "DY At Cost:            " ++ (show $ 100.0 * ((yearlyDividend fr) / (totalCostBasis fr)))
    printLinesPretty $ (toRows3 fr) $ V.toList $ stockResults fr

  runAndParseFromStdin :: (TransactionTotals -> Maybe Request) -> Manager -> IO (Maybe FinalResult)
  runAndParseFromStdin = runAndParseGeneral (readTransactionsFromStdin)

  runAndParse :: FilePath -> (TransactionTotals -> Maybe Request) -> Manager -> IO (Maybe FinalResult)
  runAndParse f = runAndParseGeneral (readTransactionsFromFile f)

  runAndParseGeneral :: IO [(String, (Float, Float))] -> (TransactionTotals -> Maybe Request) -> Manager -> IO (Maybe FinalResult)
  runAndParseGeneral transactionsIO tmr m = do
    transactions <- transactionsIO
    let portfolio  = mkPortfolio transactions
        mr         = tmr portfolio
    maybe (return Nothing) (mkRun m transactions portfolio googleResponseToQuotes) mr where


  mkRun :: Manager -> [(String, (Float, Float))] -> TransactionTotals -> (Response ByteString -> Maybe (V.Vector Quote)) -> Request -> IO (Maybe FinalResult)
  mkRun m transactions portfolio parser r = do
    response  <- httpLbs r m
    return $ do
     v <- parser response
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
  yqlUrl2 tt = parseRequest $ "http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.quotes%20where%20symbol%20in%20(" ++ (symbolsString tt) ++ ")%0A%09%09&format=json&diagnostics=true&env=http%3A%2F%2Fdatatables.org%2Falltables.env"

  googleUrl :: TransactionTotals -> Maybe Request
  googleUrl tt = parseRequest $ "http://finance.google.com/finance/info?client=ig&q=" ++ (intercalate "," $ symbols tt)

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

  parseQuotesGoogle :: Value -> Parser (V.Vector QuoteRaw)
  parseQuotesGoogle v = do
    arr     <- parseArray v
    objects <- parseObjects arr
    tuples  <- traverse parseQuoteGoogle objects
    return tuples

  parseQuote :: Object -> Parser QuoteRaw
  parseQuote v = QuoteRaw <$> v .: "Symbol" <*> v .: "PreviousClose" <*> v .: "LastTradePriceOnly" <*> v .: "DividendShare" <*> v.: "EarningsShare"

  parseQuoteGoogle :: Object -> Parser QuoteRaw
  parseQuoteGoogle v = QuoteRaw <$> v .: "t" <*> v .: "pcls_fix" <*> v .: "l" <*> (pure Nothing) <*> (pure Nothing)

  responseToQuotes :: Response ByteString -> Maybe (V.Vector Quote)
  responseToQuotes r = (parseMyResult r) >>= (toQuotes)

  googleResponseToQuotes :: Response ByteString -> Maybe (V.Vector Quote)
  googleResponseToQuotes r = (parseGoogle r) >>= (toQuotes)

  parseGoogle :: Response ByteString -> Maybe MyResult
  parseGoogle r = parseGoogleBS $ responseBody r

  parseGoogleBS :: ByteString -> Maybe MyResult
  parseGoogleBS bs = do
    result <- decode (C.filter (\c -> c /= '/') bs) :: Maybe Value
    flip parseMaybe result parseQuotesGoogle

  parseMyResult :: Response ByteString -> Maybe MyResult
  parseMyResult r = do result <- decode (responseBody r)
                       flip parseMaybe result $ \obj -> do
                         query   <- obj .: "query"
                         results <- query .: "results"
                         quote   <- results .: "quote"
                         tuples <- parseQuotes quote
                         return tuples
