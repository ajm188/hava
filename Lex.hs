module Lex
( tokenize
) where

import Text.Regex

mkRegex' :: String -> Regex
mkRegex' s = mkRegexWithOpts ("^" ++ s) True True

opRe = mkRegex' ('(':'\\':'+':'|':'\\':'*':"|=|-|/|%)")
keywordRe = mkRegex' "(if|else)"
boolRe = mkRegex' "(true|false)"
intRe = mkRegex' "([[:digit:]]+)"  -- start with nonnegative integers for now
identRe = mkRegex' "[_a-z][_a-zA-Z0-9]*"
lineEnding = mkRegex ";"
delim = mkRegex' ('(':'\\':'(':'|':'\\':')':'|':"{|})")

tokenize :: String -> [String]
tokenize [] = []
tokenize s =
    case matchRegexAll lineEnding normal of
        Nothing -> tokenizeLine normal
        Just(x, _, xs, _) -> tokenizeLine x ++ ";":tokenize xs
    where normal = removeWeirdness s

tokenizeLine :: String -> [String]
tokenizeLine [] = []
tokenizeLine line@(x:xs)
    | x == ' ' || x == '\n' = tokenizeLine xs
    | otherwise = tok:tokenizeLine rem
    where (tok, rem) = subtokenizeLine [opRe
                                       ,keywordRe
                                       ,delim
                                       ,boolRe
                                       ,intRe
                                       ,identRe] line

subtokenizeLine :: [Regex] -> String -> (String, String)
subtokenizeLine [] _ = ("", "")
subtokenizeLine (x:xs) s =
    case result of Nothing -> subtokenizeLine xs s
                   (Just(_, m, r, _)) -> (m, r)
    where result = matchRegexAll x s

removeWeirdness :: String -> String
removeWeirdness [] = []
removeWeirdness s = subRegex (mkRegex "(\t|\v)") s " "
