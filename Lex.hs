module Lex
( Token(..)
, tokenize
) where

import Text.Regex hiding (mkRegex)

data Token =
    TBool String |
    TInt String |
    TOp String |
    TIdent String |
    TErr
    deriving Show

mkRegex :: String -> Regex
mkRegex s = mkRegexWithOpts ("^" ++ s) True True

opRe = mkRegex ('(':'\\':'+':'|':'\\':'*':"|=|-|/|%)")
boolRe = mkRegex "(true|false)\b"
intRe = mkRegex "([[:digit:]]+)"  -- start with nonnegative integers for now
identRe = mkRegex "[_a-z][_a-zA-Z0-9]*"

tokenize :: String -> [Token]
tokenize [] = []
tokenize line@(x:xs)
    | x == ' ' || x == '\n' || x == '\t' = tokenize xs
    | otherwise = tok:tokenize rem
    where (tok, rem) = subtokenize [(opRe, TOp)
                                   ,(boolRe, TBool)
                                   ,(intRe, TInt)
                                   ,(identRe, TIdent)] line

subtokenize :: [(Regex, (String -> Token))] -> String -> (Token, String)
subtokenize [] _ = (TErr, "")
subtokenize ((x,t):xs) s =
    case result of Nothing -> subtokenize xs s
                   (Just(_, m, r, _)) -> (t m, r)
    where result = matchRegexAll x s
