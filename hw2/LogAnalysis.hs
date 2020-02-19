{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

--
-- Exercise 1
--

parseMessage :: String -> LogMessage
parseMessage message = parseMessageParts (words message)

parseMessageParts :: [String] -> LogMessage
parseMessageParts ("E":severity:timestamp:parts) = LogMessage (Error $ read severity) (read timestamp) (unwords parts)
parseMessageParts ("I":timestamp:parts) = LogMessage Info (read timestamp) (unwords parts)
parseMessageParts ("W":timestamp:parts) = LogMessage Warning (read timestamp) (unwords parts)
parseMessageParts parts = Unknown (unwords parts)

parse :: String -> [LogMessage]
parse messages = map parseMessage (lines messages)

