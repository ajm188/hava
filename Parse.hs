module Parse
( parseStmts
) where

import Lex
import Utils

data Stmt = ExprStmt Expr | AssnStmt Expr BinOp Expr
    deriving Show

data Expr =
    Const Int |
    Var String |
    BinExpr Expr BinOp Expr
    deriving Show

data BinOp =
    Add |
    Sub |
    Mul |
    Div |
    Mod |
    Eql
    deriving Show

exprOps = [TOp "+", TOp "-"]
termOps = [TOp "*", TOp "/", TOp "%"]

parseStmts :: [[Token]] -> [Stmt]
parseStmts [] = []
parseStmts (s:ss) = (parseStmt s):(parseStmts ss)

parseStmt :: [Token] -> Stmt
parseStmt ((TIdent v):(TOp "="):expr) = AssnStmt (Var v) Eql (parseExpr expr)
parseStmt ts = ExprStmt $ parseExpr ts

parseExpr :: [Token] -> Expr
parseExpr tokens = searchWithinParens tokens exprOps parseExpr parseTerm

parseTerm :: [Token] -> Expr
parseTerm tokens = searchWithinParens tokens termOps parseTerm parseFactor

parseFactor :: [Token] -> Expr
parseFactor ((TIdent v):[]) = Var v
parseFactor ((TInt i):[]) = Const $ read i
parseFactor ((TDelim "("):tokens) =
    case beforeLast [TDelim ")"] tokens of
        Just(inner) -> parseExpr inner
parseFactor ts = parseExpr ts

searchWithinParens :: [Token] -> [Token] -> ([Token] -> Expr) -> ([Token] -> Expr) -> Expr
searchWithinParens tokens ops f f' =
    case split [TDelim "("] tokens of
        Nothing ->
            case splitFirst ops tokens of
                Nothing -> f' tokens
                Just([l, [op], r]) -> BinExpr (f l) (binOp op) (f r)
        Just(([], r)) ->
            case splitLast [TDelim ")"] tokens of
                Just(l, []) -> f' tokens
                Just(l, r) ->
                    case splitFirst ops r of
                        Nothing -> f' tokens
                        Just([l', [op], r']) ->
                            BinExpr (f $ l ++ l') (binOp op) (f r')
        Just(l, r) ->
            case splitFirst ops l of
                Nothing -> f' tokens
                Just([l', [op], r']) -> BinExpr (f l') (binOp op) (f $ r' ++ r)

binOp :: Token -> BinOp
binOp (TOp "+") = Add
binOp (TOp "-") = Sub
binOp (TOp "*") = Mul
binOp (TOp "/") = Div
binOp (TOp "%") = Mod
