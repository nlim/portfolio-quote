module ChanTesting where
  import Control.Monad.STM
  import Control.Concurrent
  import Control.Concurrent.STM.TChan
  import Control.Concurrent.STM.TVar

  oneSecond = 1000000

  writerThread :: TChan Int -> IO ()
  writerThread chan = do
    atomically $ writeTChan chan 1
    threadDelay oneSecond
    atomically $ writeTChan chan 2
    threadDelay oneSecond
    atomically $ writeTChan chan 3
    threadDelay oneSecond

  readerThread :: Int -> TChan Int -> IO ()
  readerThread i chan = do
    newInt <- atomically $ readTChan chan
    let total = newInt + i
    putStrLn $ "new value: " ++ show newInt ++ " total: " ++ show total
    readerThread total chan

  runChanStuff = do
    tvar <- newTVarIO 0
    chan <- atomically $ newTChan
    forkIO $ printer tvar
    forkIO $ readerThread 0 chan
    mapM_ (\_ -> forkChild tvar $ writerThread chan) [1..10]
    waitForWorkers tvar
    putStrLn $ "Done waiting for workers"

  printer :: (TVar Int) -> IO ()
  printer tvar = do
    threadDelay $ 1 * oneSecond
    n <- atomically $ readTVar tvar
    putStrLn ("Num Working: " ++ show n)
    printer tvar


  waitForWorkers :: (TVar Int) -> IO ()
  waitForWorkers tvar = do
    c <- atomically $ readTVar tvar
    if (c <= 0) then return () else waitForWorkers tvar

  modifyMVar2 :: (TVar a) -> (a -> a) -> IO ()
  modifyMVar2 m f = atomically $ do
    a <- readTVar m
    writeTVar m $ f a

  forkChild :: (TVar Int) -> IO () -> IO ThreadId
  forkChild tvar io = do
    _ <- modifyMVar2 tvar (+1)
    forkFinally io (\_ -> modifyMVar2 tvar (\i -> i - 1))
