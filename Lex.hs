module Lex
( Token(..)
, tokenize
, matchingDelim
) where

import Text.Regex hiding (mkRegex)

data Token =
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

mkRegex :: String -> Regex
mkRegex s = mkRegexWithOpts ("^" ++ s) True True

opRe = mkRegex ('(':'\\':'+':'|':'\\':'*':"|=|-|/|%)")
boolRe = mkRegex "(true|false)\b"
intRe = mkRegex "([[:digit:]]+)"  -- start with nonnegative integers for now
identRe = mkRegex "[_a-z][_a-zA-Z0-9]*"
lineEnding = mkRegexWithOpts ";" True True -- can't use my override here
delim = mkRegex ('(':'\\':'(':'|':'\\':')':'|':"{|})")

tokenize :: String -> [[Token]]
tokenize [] = []
tokenize s = case match of Nothing -> [tokenizeLine s]
                           Just(x, _, xs, _) -> tokenizeLine x:tokenize xs
    where match = matchRegexAll lineEnding s

tokenizeLine :: String -> [Token]
tokenizeLine [] = []
tokenizeLine line@(x:xs)
    | x == ' ' || x == '\n' || x == '\t' = tokenizeLine xs
    | otherwise = tok:tokenizeLine rem
    where (tok, rem) = subtokenizeLine [(opRe, TOp)
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
