module Lex
( Token(..)
{-
, Literal(..)
, Keyword(..)
-}
) where

import Text.Regex hiding (mkRegex)

data Token =
    BoolToken Bool |
    IntegerToken Int
    deriving Show

boolRegex = mkRegex "(True|False)"
integerRegex = mkRegex "([[:digit:]]+)"  -- start with nonnegative integers for now
whitespaceRegex = mkRegex "([[:space:]]+)"

mkRegex :: String -> Regex
mkRegex s = mkRegexWithOpts ("^" ++ s) True True

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList (Nothing) = []

tokenizeLine :: String -> [Token]
tokenizeLine line = case result of ((_,[]):(_,[]):(_,[]):_) -> []
                                   ((_,[]):(_,[]):(x,y):_) -> tokenizeLine x
                                   ((_,[]):(x,y):_) -> (processMatches IntegerToken y) ++ tokenizeLine x
                                   ((x,y):_) -> (processMatches BoolToken y) ++ tokenizeLine x
    where result = subtokenizeLine [boolRegex, integerRegex, whitespaceRegex] line

subtokenizeLine :: [Regex] -> String -> [(String, [String])]
subtokenizeLine [] _ = []
subtokenizeLine (r:rs) s = case result of [] -> ("",[]):subtokenizeLine rs s
                                          [(_, _, rem, matches)] -> (rem, matches):[("",[]) | s' <- rs]
    where result = maybeToList $ matchRegexAll r s

processMatches :: (Read a) => (a -> Token) -> [String] -> [Token]
processMatches t m = [t (read m') | m' <- m]
