module Utils
( splitFirst
, beforeFirst
, findFirst
, afterFirst
) where

import Control.Applicative

splitFirst :: (Eq a) => [a] -> [a] -> Maybe [[a]]
splitFirst ms xs =
    case before of
        Nothing -> Nothing
        Just(b) ->
            case match of
                Just(m) ->
                    case after of
                        Nothing -> Just [b, [m], []]
                        Just(a) -> Just [b, [m], a]
    where before = beforeFirst ms xs
          match = findFirst ms xs
          after = afterFirst ms xs

beforeFirst :: (Eq a) => [a] -> [a] -> Maybe [a]
beforeFirst _ [] = Nothing
beforeFirst ms (x:xs)
    | any (==x) ms = Just []
    | otherwise = Just(\l -> x:l) <*> beforeFirst ms xs

findFirst :: (Eq a) => [a] -> [a] -> Maybe a
findFirst _ [] = Nothing
findFirst ms (x:xs)
    | any (==x) ms = Just x
    | otherwise = findFirst ms xs

afterFirst :: (Eq a) => [a] -> [a] -> Maybe [a]
afterFirst _ [] = Nothing
afterFirst ms (x:xs)
    | any (==x) ms = Just xs
    | otherwise = afterFirst ms xs
