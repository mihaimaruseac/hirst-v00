module Users where

import Data.ByteString
import Data.ByteString.Char8 as BSC
import Data.Conduit
import Data.Conduit.List
import Prelude as P

import LogReader

collectUsers :: FilePath -> IO ()
collectUsers logDir = do
  logs <- readLogs logDir
  results <- runResourceT $ logs $$ processLogs
  print results
--  P.mapM_ (P.mapM_  (P.putStrLn . ) results

processLogs = awaitForever forEachLine =$ consume

forEachLine l = do
  yield $ BSC.words l
