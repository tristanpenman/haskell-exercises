{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

--
-- Exercise 1
--
-- Parse a series of log messages
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

--
-- Exercise 2
--
-- Insert a LogMessage into a MessageTree, giving a new MessageTree containing the message
--

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message tree =
  let (LogMessage _ ts1 _) = message
      (Node left middle right) = tree
      (LogMessage _ ts2 _) = middle
  in if ts1 < ts2 then (Node (insert message left) middle right)
     else (Node left middle (insert message right))
