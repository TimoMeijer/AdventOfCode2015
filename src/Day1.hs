module Day1 where

import Data.List

data Direction =  Up | Down

solve1_1 :: (Monad m) => String -> m Int
solve1_1 i = do
    l <- parse i
    return $ foldl addDirection 0 l

solve1_2 :: (Monad m) => String -> m Int
solve1_2 i = do
    l <- parse i
    let x = elemIndex (-1) $ scanl addDirection 0 l
    case x of
        Nothing -> fail "Santa never enters the basement"
        Just r -> return r

addDirection :: Int -> Direction -> Int
addDirection x Up = x + 1
addDirection x Down = x - 1

parse :: (Monad m) => String -> m [Direction]
parse = mapM parseOne

parseOne :: (Monad m) => Char -> m Direction
parseOne '(' = return Up
parseOne ')' = return Down
parseOne x   = fail $ "Unknown token: " ++ show x
