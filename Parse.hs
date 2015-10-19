module Parse
( parse
, AST(..)
, Token(..)
) where

import Lex
import Text.Regex.Base
import Text.Regex.Posix

keywords = ["if", "else"]
exprOps = ["+", "-"]
termOps = ["*", "/", "%"]

data Token =
    Ident String |
    Number String |
    Add | Sub |
    Mul | Div | Mod
    deriving Show

data AST =
    Empty |
    Leaf Token |
    Assign Token AST |
    BinEx AST AST AST |
    If AST AST AST |
    While AST AST |
    Block [AST]
    deriving Show

-- HELPERS --
(|>) :: a -> (a -> b) -> b
(|>) f g = g f

peek :: [a] -> Maybe a
peek [] = Nothing
peek (x:xs) = Just x

consume :: [a] -> [a]
consume [] = []
consume (x:xs) = xs

isDigit :: String -> Bool
isDigit d = d =~ "[0-9]"

isIdent :: String -> Bool
isIdent i = not (elem i keywords) && i =~ "[a-zA-Z_][a-zA-Z0-9_]*"

operator :: String -> Token
operator "+" = Add
operator "-" = Sub
operator "*" = Mul
operator "/" = Div
operator "%" = Mod

-- MAIN PARSING ROUTINE
parse :: [String] -> Maybe [AST]
parse [] = Nothing
parse tokens = case stmts tokens of ([], a) -> Just a
                                    (t, _)  -> Nothing

-- THE GRAMMAR --
stmts :: [String] -> ([String], [AST])
stmts [] = ([], [])
stmts (";":tokens) = stmts tokens
stmts tokens =
    let (t, a) = stmt (tokens, Empty)
        (t', as) = stmts t
    in (t', a:as)

stmt :: ([String], AST) -> ([String], AST)
stmt ([], ast) = ([], ast)
stmt (tokens@("if":_), ast) = ifstmt (tokens, ast)
stmt (tokens@("while":_), ast) = whilestmt (tokens, ast)
stmt (tokens@(t:"=":_), ast)
    | isIdent t = assignStmt (tokens, ast)
stmt (tokens, ast) = (tokens, ast)

ifstmt :: ([String], AST) -> ([String], AST)
ifstmt ([], ast) = ([], ast)
ifstmt (("if":tokens), ast) =
    case tokens of
        ("(":tokens') ->
            let (t, cond) = expr (tokens', Empty)
            in case t of
                (")":"{":t') ->
                    let (t1, trueBranch) = block (t', Empty)
                    in case t1 of
                        ("}":t1') ->
                            case t1' of
                                ("else":"if":t1'') ->
                                    let (t2, falseBranch) = ifstmt (("if":t1''), Empty)
                                    in (t2, If cond trueBranch falseBranch)
                                ("else":"{":t1'') ->
                                    let (t2, falseBranch) = block (t1'', Empty)
                                    in case t2 of
                                        ("}":t2') ->
                                                (t2', If cond trueBranch falseBranch)
                                    t1'' -> (t1'', If cond trueBranch Empty)
ifstmt (tokens, ast) = (tokens, ast)

whilestmt :: ([String], AST) -> ([String], AST)
whilestmt ([], ast) = ([], ast)
whilestmt (("while":tokens), ast) =
    case tokens of
        ("(":tokens') ->
            let (t, cond) = expr (tokens', Empty)
            in case t of
                (")":"{":t') ->
                    let (t1, body) = block (t', Empty)
                    in case t1 of
                        ("}":t1') -> (t1', While cond body)

assignStmt :: ([String], AST) -> ([String], AST)
assignStmt ([], ast) = ([], ast)
assignStmt (tokens@(t:"=":ts), ast)
    | isIdent t =
        let (t', a') = expr (ts, Empty)
        in (t', Assign (Ident t) a')
assignStmt (tokens, ast) = (tokens, ast)

block :: ([String], AST) -> ([String], AST)
block (tokens, ast) =
    let (t, as) = block' tokens
    in (t, Block as)
    {- block' is nearly the same as stmts, but also terminate on "}" -}
    where block' [] = ([], [])
          block' (";":tokens) = block' tokens
          block' tokens@("}":_) = (tokens, [])
          block' tokens =
            let (t, a) = stmt (tokens, Empty)
                (t', as) = block' t
            in (t', a:as)

expr :: ([String], AST) -> ([String], AST)
expr ([], ast) = ([], ast)
expr (tokens, ast) = (tokens, ast) |> term |> exprOp

term :: ([String], AST) -> ([String], AST)
term ([], ast) = ([], ast)
term (tokens, ast) = (tokens, ast) |> factor |> termOp

factor :: ([String], AST) -> ([String], AST)
factor ([], ast) = ([], ast)
factor (tokens, ast)
    | Just d <- peek tokens
    , isDigit d = ((consume tokens), Leaf (Number d))
    | Just i <- peek tokens
    , isIdent i = ((consume tokens), Leaf (Ident i))
    | Just p <- peek tokens
    , p == "(" = let (t, a) = expr ((consume tokens), Empty)
                 in case peek t of
                    Just ")" -> ((consume t), a)

exprOp :: ([String], AST) -> ([String], AST)
exprOp (tokens, ast) =
    case peek tokens of
        Just o | elem o exprOps -> let (t, a) = ((consume tokens), ast) |> term
                                   in (t, BinEx ast (Leaf $ operator o) a) |> exprOp
        otherwise -> (tokens, ast)

termOp :: ([String], AST) -> ([String], AST)
termOp (tokens, ast) =
    case peek tokens of
        Just o | elem o termOps -> let (t, a) = ((consume tokens), ast) |> factor
                                   in (t, BinEx ast (Leaf $ operator o) a) |> termOp
        otherwise -> (tokens, ast)
