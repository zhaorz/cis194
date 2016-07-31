{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log


parseMessage :: String -> LogMessage
parseMessage s = case (words s) of
  ("I":time:msg)       -> LogMessage Info (read time :: Int) (unwords msg)
  ("W":time:msg)       -> LogMessage Warning (read time :: Int) (unwords msg)
  ("E":level:time:msg) -> LogMessage (Error (read level :: Int))
                          (read time :: Int) (unwords msg)
  _                    -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ time _) (Node l m'@(LogMessage _ time' _) r) =
  if time < time'
  then Node (insert m l) m' r
  else Node l m' (insert m r)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m:ms) =
  let t = build ms
  in insert m t

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = (inOrder l) ++ (m:(inOrder r))


-- whatWentWrong ms => L where L consists of the messages in ms
--                     with an error severity of at least 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong (m:ms) = case m of
  (LogMessage (Error level) _ msg) -> if level >= 50
    then msg:(whatWentWrong ms)
    else (whatWentWrong ms)
  _   -> (whatWentWrong ms)
