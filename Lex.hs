module Lex
( Token(..)
, tokenize
, matchingDelim
) where

import Text.Regex

data Token =
    TKeyword String |
    TBool String |
    TInt String |
    TOp String |
    TIdent String |
    TDelim String |
    TEOL |
    TErr
    deriving (Eq, Show)

matchingDelim :: Token -> Token
matchingDelim (TDelim "(") = TDelim ")"
matchingDelim (TDelim "{") = TDelim "}"

mkRegex' :: String -> Regex
mkRegex' s = mkRegexWithOpts ("^" ++ s) True True

opRe = mkRegex' ('(':'\\':'+':'|':'\\':'*':"|=|-|/|%)")
keywordRe = mkRegex' "(if|else)"
boolRe = mkRegex' "(true|false)"
intRe = mkRegex' "([[:digit:]]+)"  -- start with nonnegative integers for now
identRe = mkRegex' "[_a-z][_a-zA-Z0-9]*"
lineEnding = mkRegex ";"
delim = mkRegex' ('(':'\\':'(':'|':'\\':')':'|':"{|})")

tokenize :: String -> [Token]
tokenize [] = []
tokenize s =
    case matchRegexAll lineEnding normal of
        Nothing -> tokenizeLine normal
        Just(x, _, xs, _) -> tokenizeLine x ++ TEOL:tokenize xs
    where normal = removeWeirdness s

tokenizeLine :: String -> [Token]
tokenizeLine [] = []
tokenizeLine line@(x:xs)
    | x == ' ' || x == '\n' = tokenizeLine xs
    | otherwise = tok:tokenizeLine rem
    where (tok, rem) = subtokenizeLine [(opRe, TOp)
                                       ,(keywordRe, TKeyword)
                                       ,(delim, TDelim)
                                       ,(boolRe, TBool)
                                       ,(intRe, TInt)
                                       ,(identRe, TIdent)] line

subtokenizeLine :: [(Regex, (String -> Token))] -> String -> (Token, String)
subtokenizeLine [] _ = (TErr, "")
subtokenizeLine ((x,t):xs) s =
    case result of Nothing -> subtokenizeLine xs s
                   (Just(_, m, r, _)) -> (t m, r)
    where result = matchRegexAll x s

removeWeirdness :: String -> String
removeWeirdness [] = []
removeWeirdness s = subRegex (mkRegex "(\t|\v)") s " "
