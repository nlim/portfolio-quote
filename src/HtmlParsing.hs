{-# LANGUAGE OverloadedStrings #-}

module HtmlParsing where
  import Network.HTTP.Conduit (simpleHttp)
  import Prelude hiding (concat)
  import qualified Prelude as Pr (concat)
  import Data.Text (concat)
  import Text.HTML.DOM (parseLBS)
  import Data.Maybe (mapMaybe, listToMaybe, isJust)
  import Text.XML.Cursor (child, Cursor, content, check, attribute, attributeIs, element, fromDocument, ($//), (&//), (&.//), (&/), (&|))
  import Debug.Trace (traceShowId)
  import Data.List (filter, intercalate)
  import TransactionLoading (writeFromTChan, writeAllResults, readUntilEnd)
  import ChanTesting
  import Numeric (showFFloat)
  import Data.List.Split (splitOn)
  import qualified Control.Monad.STM as CMSTM
  import Control.Concurrent
  import Control.Concurrent.STM.TChan
  import Control.Concurrent.STM.TVar


  cursorFor :: String -> IO Cursor
  cursorFor u = do
      page <- simpleHttp u
      return $ fromDocument $ parseLBS page

  extractData = concat . content

  hasContent :: Cursor -> Bool
  hasContent c = (length (content c)) > 0

  type ExportValue = (String, String, Float, Float, Float, Float)

  toCsvLine :: ExportValue -> String
  toCsvLine (s, s2, f, f2, f3, f4) = intercalate "," [s, s2, (showFFloat Nothing f) "", (showFFloat Nothing f2) "", (showFFloat Nothing f3) "", (showFFloat Nothing f4) ""]

---outputFile = "/Users/nlim/Desktop/results.txt"
--
  exportData :: String -> String -> IO ()
  exportData symbolFile outputFile = do
    allsymbols <- lookupAllSymbols symbolFile
    let symbols = allsymbols --take 200 allsymbols
    chan <- CMSTM.atomically $ newTChan
    tvar <- newTVarIO 0
    forkIO $ writeFromTChan toCsvLine outputFile chan
    let mappedOut = splitNPieces 200 symbols
    mapM_ (\ss -> forkChild tvar $ writeOutAllResults chan ss) mappedOut
    waitForWorkers tvar
    putStrLn $ "Done waiting for workers"


  writeOutAllResults :: TChan ExportValue -> [String] -> IO ()
  writeOutAllResults tchan [] = return ()
  writeOutAllResults tchan (s:ss) = (writeOutResult tchan s) >> (writeOutAllResults tchan ss)

  writeOutResult :: TChan ExportValue -> String -> IO ()
  writeOutResult tchan symbol = do
    evM <- yieldData2 symbol
    case evM of
      Just ev -> CMSTM.atomically $ writeTChan tchan ev
      Nothing -> return ()


  splitNPieces :: Int -> [a] -> [[a]]
  splitNPieces n as = fmap (\i -> modList i n as) [0..(n-1)]

  modList :: Int -> Int -> [a] -> [a]
  modList n t as = fmap snd $ Prelude.filter (\tuple -> (mod (fst tuple) t) == n) $ zip [0..] as

  lookupAllSymbolsWithIndex :: String -> IO [(Int, String)]
  lookupAllSymbolsWithIndex symbolFile = do
    symbols <- lookupAllSymbols symbolFile
    return $ zip [0..] symbols

  lookupAllSymbols :: String -> IO [String]
  lookupAllSymbols symbolFile = do
    lines <- readUntilEnd symbolFile
    let splitOut = fmap (splitOn ",") lines
        symbols  = fmap ( (Prelude.filter (/='/')) . (Prelude.filter (/=' ')) . stripQuotes. head) splitOut
    return $ symbols

  filterDataClass :: Cursor -> Bool
  filterDataClass c = any (isDataClass) v where
    v = (attribute "class") c

  isDataClass :: (Show a) => a -> Bool
  isDataClass t = s == "\"yfnc_tabledata1\"" where
    s = show t

  dataUrl :: String -> String
  dataUrl s = "http://finance.yahoo.com/q/ks?s=" ++ s ++ "+Key+Statistics"

  stripQuotes :: String -> String
  stripQuotes = Prelude.filter (/= '\"')

  yieldData2  :: String -> IO (Maybe (String, String, Float, Float, Float, Float))
  yieldData2 s = do
    vMaybe     <- enterpriseValueAndMarketCapDeq s
    oiMaybe     <- lookupIncome s
    sectorMaybe <- lookupSector s
    return $ do
      (ev, mc, deq) <- vMaybe
      oi            <- oiMaybe
      sector        <- sectorMaybe
      return (s, sector, ev, mc, oi, deq)

  maybeRead :: Read a => String -> Maybe a
  maybeRead = fmap fst . listToMaybe . reads

  maybeFloat :: String -> Maybe Float
  maybeFloat = maybeRead

  incomeUrl :: String -> String
  incomeUrl s  = "http://finance.yahoo.com/q/is?s=" ++ s ++ "+Income+Statement&annual"

  incomeUrl2 :: String -> String
  incomeUrl2 s  = "http://finance.yahoo.com/q/is?s=" ++ s

  balanceSheetUrl :: String -> String
  balanceSheetUrl s = "http://finance.yahoo.com/q/bs?s=" ++ s ++ "+Balance+Sheet"

  removeCrap1 :: String -> String
  removeCrap1 s = Pr.concat $ splitOn "\\n" s

  removeCrap2 :: String -> String
  removeCrap2 s = Pr.concat $ splitOn "\\160" s

  removeCrap = removeCrap1 . removeCrap2

  lookupIncome s = do
    incomeM <- lookupIncome2 s
    case incomeM of
      Just i  -> return $ Just i
      Nothing -> lookupIncome1 s

  lookupIncome1 s = do
    cursor <- cursorFor $ incomeUrl s
    let css = cursor $// (element "td" &.// element "strong" &.// check hasContent) &| extractData
    let filtered = fmap (removeCrap .  (Prelude.filter (/='"')) . (Prelude.filter (/=' ')) . show) css
    return $ do
      c <- listToMaybe $ (drop 10) filtered
      let is = Prelude.filter (/= ',') c
      f <- parseFloatWithMultiplier is
      return $ 1000 * f

  lookupIncome2 s = do
    cursor <- cursorFor $ incomeUrl2 s
    let css = cursor $// (element "td" &.// element "strong" &.// check hasContent) &| extractData
    let filtered = fmap (removeCrap .  (Prelude.filter (/='"')) . (Prelude.filter (/=' ')) . show) css
        noCommas = fmap (Prelude.filter (/= ',')) filtered
    return $ do
      q1 <- floatAt 13 noCommas
      q2 <- floatAt 14 noCommas
      q3 <- floatAt 15 noCommas
      q4 <- floatAt 16 noCommas
      return $ (1000.0 * (q1 + q2 + q3 + q4))


  lookupBalanceSheet s = do
    cursor <- cursorFor $ balanceSheetUrl s
    let css = cursor $// (element "td" &.// element "strong" &.// check hasContent) &| extractData
    let filtered = fmap (removeCrap .  (Prelude.filter (/='"')) . (Prelude.filter (/=' ')) . show) css
    let noCommas = fmap (Prelude.filter (/= ',')) filtered
    let withNs   = zip [1..] noCommas
    return $ do
      q1 <- fmap (1000.0 * ) $ floatAt 4 noCommas
      q2 <- fmap (1000.0 * ) $ floatAt 5 noCommas
      q3 <- fmap (1000.0 * ) $ floatAt 6 noCommas
      q4 <- fmap (1000.0 * ) $ floatAt 45 noCommas
      q5 <- fmap (1000.0 * ) $ floatAt 46 noCommas
      q6 <- fmap (1000.0 * ) $ floatAt 47 noCommas
      return $ ((q1, q2, q3), (q4, q5, q6))

  sectorUrl :: String -> String
  sectorUrl s = "http://finance.yahoo.com/q/in?s=" ++ s

  lookupSector :: String -> IO (Maybe String)
  lookupSector s = do
    cursor <- cursorFor $ sectorUrl s
    let css = cursor $// (element "td" &.// check filterDataClass &.// check hasContent) &| extractData
    return $ do
      c <- listToMaybe (drop 0 css)
      return $ (stripQuotes . show) c

  parseFloatWithMultiplier :: String -> Maybe Float
  parseFloatWithMultiplier s = do
   let multiplier = findMultiplier s
   f <- maybeFloat s
   return $ multiplier * f

  findMultiplier :: String -> Float
  findMultiplier s = multiple * sign where
    sign     = if (any (\c -> or [c == '(', c == ')']) s) then -1.0 else 1.0
    multiple = case (listToMaybe $ Prelude.filter (\c -> or [c == 'K', c == 'M', c == 'B']) s) of
                 (Just 'K') -> 1000.0
                 (Just 'M') -> 1000.0 * 1000.0
                 (Just 'B') -> 1000.0 * 1000.0 * 1000.0
                 otherwise  -> 1.0

  allData s = do
    cursor <- cursorFor $ dataUrl s
    let texts = cursor $// (element "td" &.// check filterDataClass &.// check hasContent) &| extractData
    return $ fmap (stripQuotes . show) texts

  floatAt i cs = do
    c <- listToMaybe (drop (i-1) cs)
    parseFloatWithMultiplier c

  enterpriseValueAndMarketCapDeq :: String -> IO (Maybe (Float, Float, Float))
  enterpriseValueAndMarketCapDeq s = lookupMultipleData s $ \css -> do
    mc  <- floatAt 1 css
    em  <- floatAt 2 css
    deq <- floatAt 27 css
    return (em, mc, deq / 100)

  lookupMultipleData :: String -> ([String] -> Maybe a) -> IO (Maybe a)
  lookupMultipleData s f = do
    css <- allData s
    return $ f css
