{-# LANGUAGE ScopedTypeVariables #-}
import System.IO
import System.Timeout
import Network
import Control.Exception
import Control.Concurrent
import Control.Monad
import Safe
import System.Time hiding (diffClockTimes)
import System.Environment
import Control.Concurrent.Chan
import System.Exit

_RECEIVE_TIMEOUT_ :: Int
_RECEIVE_TIMEOUT_ = 2 * 1000 * 1000 -- in microseconds

type TimeSpan = Integer -- microseconds

diffClockTimes :: ClockTime -> ClockTime -> TimeSpan
diffClockTimes (TOD s1 p1) (TOD s0 p0) =
    (picoseconds p1 + seconds s1) -
    (picoseconds p0 + seconds s0)
    where
      picoseconds p = p `div` (1000 * 1000)
      seconds s = s * 1000000

ignoreExc :: IO () -> IO ()
ignoreExc action =
   action `catch` (\(e::SomeException) -> return ())

runClient :: String -> Int -> IO (Either String TimeSpan)
runClient host n =
    do t0 <- getClockTime
       let port = 44444
       h <- connectTo host (PortNumber port)
       hSetBuffering h LineBuffering
       res <- client h `finally` (ignoreExc $ hClose h)
       case res of
         Just err -> return (Left err)
         Nothing ->
             do t1 <- getClockTime
                return $ Right (t1 `diffClockTimes` t0)
    where
      client h =
          let loop i
                  | i > n =
                      do hPutStrLn h "end"
                         return Nothing
                  | otherwise =
                  do let size = min (n - i + 1) ((i `mod` 3) + 1)
                     x <- doChunks h i size
                     case x of
                       Just y -> return $ Just y
                       Nothing -> loop (i + size)
          in loop 1
      doChunks h start size =
          do mapM_ (\i -> write h (show i)) $ take size [start..]
             lines <- mapM (\i -> do s <- timeout _RECEIVE_TIMEOUT_ $ read h
                                     return (i, s))
                           (take size [start..])
             handleLines lines
      write h s =
          do --putStrLn s
             hPutStrLn h s
      read h =
          do --putStrLn "Reading..."
             x <- hGetLine h
             --putStrLn $ "Read " ++ x
             return x
      handleLines [] = return Nothing
      handleLines ((i, line) : rest) =
          case line of
            Nothing -> return $ Just "timeout"
            Just s ->
                case readMay s :: Maybe Int of
                  Just j ->
                      if j == 2 * i
                      then handleLines rest
                      else return $ Just ("Got " ++ show j ++ " but expected " ++ show (2 * i))
                  Nothing -> return $ Just ("Got " ++ show s)

main =
    do args <- getArgs
       case args of
         [host, nClients, nIterations]
             | Just n <- readMay nClients
             , Just m <- readMay nIterations
             ->
               do c <- newChan
                  mapM_ (\_ -> forkIO $ run c host m) [1..n]
                  writeResults c n
         _ -> putStrLn "USAGE: ./Main HOST N_CLIENTS N_ITERATIONS"
    where
      run c host m =
          do x <- runClient host m
                  `catch` (\(e::SomeException) -> return (Left (show e)))
             writeChan c x
      writeResults c n =
          if n <= 0
          then return ()
          else do x <- readChan c
                  case x of
                    Left err ->
                        do hPutStrLn stderr err
                           exitWith (ExitFailure 1)
                    _ -> return ()
                  -- putStrLn (show x)
                  writeResults c (n - 1)
