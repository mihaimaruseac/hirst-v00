module LogReader where

import Control.Applicative
import Data.Conduit
import Data.Conduit.List
import Data.Conduit.Binary as DCB
import Data.List as DL
import System.Directory
import Prelude as P

readLogs logDir = do
  files <- P.filter isLogFile <$> getDirectoryContents logDir
  return $ sourceList (P.take 1 files) $= awaitForever (\f -> getLogCnt $ logDir ++ f)

isLogFile :: FilePath -> Bool
isLogFile = DL.isSuffixOf ".log"

getLogCnt fName = sourceFile fName =$= DCB.lines
