module Lex
( Token(..)
, tokenize
) where

import Text.Regex hiding (mkRegex)

data Token =
    TBool String |
    TInt String |
    TErr
    deriving Show

mkRegex :: String -> Regex
mkRegex s = mkRegexWithOpts ("^" ++ s) True True

boolRe = mkRegex "(true|false)"
intRe = mkRegex "([[:digit:]]+)"  -- start with nonnegative integers for now

tokenize :: String -> [Token]
tokenize [] = []
tokenize line@(x:xs)
    | x == ' ' || x == '\n' || x == '\t' = tokenize xs
    | otherwise = token:tokenize remainder
    where (token, remainder) = subtokenize [(boolRe, TBool), (intRe, TInt)] line

subtokenize :: [(Regex, (String -> Token))] -> String -> (Token, String)
subtokenize [] _ = (TErr, "")
subtokenize ((x,t):xs) s = case result of Nothing -> subtokenize xs s
                                          (Just(_, m, r, _)) -> (t m, r)
    where result = matchRegexAll x s
