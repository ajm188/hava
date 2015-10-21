module Lex
( tokenize
, main
) where

import Text.Regex

import IOUtils

mkRegex' :: String -> Regex
mkRegex' s = mkRegexWithOpts ("^" ++ s) True True

compOp = mkRegex' "(==|!=|>|>=|<|<=)"
boolOp = mkRegex' "(&&|\\|\\|)"
arithOp = mkRegex' ('(':'\\':'+':'|':'\\':'*':"|=|-|/|%)")
keyword = mkRegex' "(if|else)"
bool = mkRegex' "(true|false)"
int = mkRegex' "([[:digit:]]+)"  -- start with nonnegative integers for now
ident = mkRegex' "[_a-z][_a-zA-Z0-9]*"
delim = mkRegex' ('(':'\\':'(':'|':'\\':')':'|':"{|})")

regexes = [compOp, boolOp, arithOp, keyword, delim, bool, int, ident]

lineEnding = mkRegex ";"

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
    where (tok, rem) = subtokenizeLine regexes line

subtokenizeLine :: [Regex] -> String -> (String, String)
subtokenizeLine [] _ = ("", "")
subtokenizeLine (x:xs) s =
    case result of Nothing -> subtokenizeLine xs s
                   (Just(_, m, r, _)) -> (m, r)
    where result = matchRegexAll x s

removeWeirdness :: String -> String
removeWeirdness [] = []
removeWeirdness s = subRegex (mkRegex "(\t|\v)") s " "

main = do
    text <- getInput
    print $ tokenize $ concat text
