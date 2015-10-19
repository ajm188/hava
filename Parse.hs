{-
 - THE GRAMMAR
 -
 - S -> Stmts
 -
 - Stmts -> Stmt Stmts
 - Stmts -> Stmt
 -
 - Stmt -> Assign
 - Stmt -> If
 - Stmt -> If <else> If
 -
 - Assign -> <ident> <=> Expr
 -
 - If -> <if> <(> Expr <)> Block
 -
 - Block -> <{> Stmts <}>
 -
 - Expr -> Term
 - Expr -> Expr <+> Expr | Expr <-> Expr
 -
 - Term -> Factor
 - Term -> Term <*> Term | Term </> Term | Term <%> Term
 -
 - Factor -> <number>
 - Factor -> <(> Expr <)>
-}
module Parse
( parse
, AST(..)
, Token(..)
) where

import Text.Regex.Base
import Text.Regex.Posix

data Token =
    Number String |
    Add | Mul
    deriving Show

data AST =
    Empty |
    Leaf Token |
    BinEx AST AST AST |
    If Token Token AST Token AST |
    IfElse Token Token AST Token AST Token
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

-- MAIN PARSING ROUTINE
parse :: [String] -> Maybe AST
parse [] = Nothing
parse tokens = case expr (tokens, Empty) of ([], a) -> Just a
                                            (t, _)  -> Nothing

-- THE GRAMMAR --
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
    | Just p <- peek tokens
    , p == "(" = let (t, a) = expr ((consume tokens), Empty)
                 in case peek t of
                    Just ")" -> ((consume t), a)

exprOp :: ([String], AST) -> ([String], AST)
exprOp (tokens, ast) =
    case peek tokens of
        Just o | elem o ["+"] -> let (t, a) = ((consume tokens), ast) |> term
                                 in (t, BinEx ast (Leaf Add) a) |> exprOp
        otherwise -> (tokens, ast)

termOp :: ([String], AST) -> ([String], AST)
termOp (tokens, ast) =
    case peek tokens of
        Just o | elem o ["*"] -> let (t, a) = ((consume tokens), ast) |> factor
                                 in (t, BinEx ast (Leaf Mul) a) |> termOp
        otherwise -> (tokens, ast)
