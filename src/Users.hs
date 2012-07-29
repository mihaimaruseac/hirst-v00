{-# LANGUAGE RecordWildCards #-}

module Users where

import Data.ByteString.Char8 as BSC
import Data.Conduit
import Data.Conduit.List as DCL
import Data.Set as S
import Prelude as P
import Debug.Trace

import LogReader

data SeenUsers = SU
  { nicksSeen :: Set String
  , nickChanges :: Set (String, String)
  } deriving (Show)

noSeenUsers = SU S.empty S.empty

collectUsers :: FilePath -> IO ()
collectUsers logDir = do
  logs <- readLogs logDir
  results <- runResourceT $ logs $$ DCL.fold processLogs noSeenUsers
  print results
  print $ size $ nicksSeen results
  print $ size $ nickChanges results

processLogs :: SeenUsers -> ByteString -> SeenUsers
processLogs seen line
  | typ == pack "***" = record content seen
  | otherwise = seen
  where
    (_:typ:content) = BSC.words line

record :: [ByteString] -> SeenUsers -> SeenUsers
record (n:ws) su@SU{..}
  | pack "has quit" `isPrefixOf` rest = recordOneNick n su
  | pack "has joined" `isPrefixOf` rest = recordOneNick n su
  | pack "has left" `isPrefixOf` rest = recordOneNick n su
  | pack "was kicked by" `isPrefixOf` rest = recordOneNick n su
  | pack "is now known as" `isPrefixOf` rest = recordChangeNick n (P.last ws) su
  | pack "sets mode" `isPrefixOf` rest = su
  | pack "changes topic" `isPrefixOf` rest = su
  | otherwise = error $ "Unknown action: " ++ show rest
  where
    rest = BSC.unwords ws

recordOneNick :: ByteString -> SeenUsers -> SeenUsers
recordOneNick n su@SU{..} = su { nicksSeen = BSC.unpack n `insert` nicksSeen }

recordChangeNick :: ByteString -> ByteString -> SeenUsers -> SeenUsers
recordChangeNick nfrom nto su@SU{..} = su
  { nicksSeen = nick `insert` (nick' `insert` nicksSeen)
  , nickChanges = (nick, nick') `insert` nickChanges
  }
  where
    nick = BSC.unpack nfrom
    nick' = BSC.unpack nto
