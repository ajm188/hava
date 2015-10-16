module Lex
( Token(..)
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

tokenizeLine :: String -> [Token]
tokenizeLine [] = []
tokenizeLine line@(x:xs)
    | x == ' ' = tokenizeLine xs
    | otherwise = token:tokenizeLine remainder
    where (token, remainder) = subtokenizeLine [(boolRe, TBool), (intRe, TInt)] line

subtokenizeLine :: [(Regex, (String -> Token))] -> String -> (Token, String)
subtokenizeLine [] _ = (TErr, "")
subtokenizeLine ((x,t):xs) s = case result of Nothing -> subtokenizeLine xs s
                                              (Just(_, m, r, _)) -> (t m, r)
    where result = matchRegexAll x s
