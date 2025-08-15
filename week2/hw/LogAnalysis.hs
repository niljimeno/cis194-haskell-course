module LogAnalysis where

import Log

analyse = do
    messages <- readFile "./sample.log"
    print $ inOrder $ build $ parse messages
    print $ whatWentWrong $ parse messages

parseMessage :: String -> LogMessage
parseMessage s = case (parseType s) of
    Nothing -> Unknown s
    Just ( err, remainder ) -> case (parseTimeStamp remainder) of
        Nothing -> Unknown s
        Just ( time, body ) -> LogMessage err time body

parseType :: String -> Maybe (MessageType, String)
parseType (x:s) = case x of
    'I' -> Just ( Info, s )
    'W' -> Just ( Warning, s )
    'E' -> case (tryRead $ head ws) of
        Nothing -> Nothing
        Just i -> Just ( Error i, unwords $ tail ws  )
        where
            ws = words s
    _ -> Nothing


parseTimeStamp :: String -> Maybe ( TimeStamp, String )
parseTimeStamp s = case (tryRead $ head ws) of
    Nothing -> Nothing
    Just t -> Just ( t, body )
    where
        ws = words s
        body = unwords $ tail ws

tryRead :: String -> Maybe Int
tryRead s = case reads s of
    [(x, _)] -> Just x
    _ -> Nothing

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert m Leaf = Node Leaf m Leaf
insert
    newMessage@(LogMessage _ newTime _ )
    fullNode@(Node left baseMessage@( LogMessage _ baseTime _ ) right)
    | newTime > baseTime = Node left baseMessage ( insert newMessage right )
    | newTime < baseTime = Node ( insert newMessage left ) baseMessage right
    | otherwise = fullNode

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m:ms) = insert m $ build ms

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) =
    (inOrder left) ++ [m] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong messages =
    map getMessage $ filter (isImportant) messages

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ m) = m

isImportant :: LogMessage -> Bool
isImportant (LogMessage (Error i) _ _) = i >= 50
isImportant _ = False
