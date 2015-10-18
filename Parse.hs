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
parseExpr ts =
    case split of
        Nothing -> parseTerm ts
        Just([l, [o], r]) -> BinExpr (parseExpr l) (binOp o) (parseExpr r)
    where split = splitFirst exprOps ts

parseTerm :: [Token] -> Expr
parseTerm ts =
    case split of
        Nothing -> parseFactor ts
        Just([l, [o], r]) -> BinExpr (parseTerm l) (binOp o) (parseTerm r)
    where split = splitFirst termOps ts

parseFactor :: [Token] -> Expr
parseFactor ((TIdent v):[]) = Var v
parseFactor ((TInt i):[]) = Const $ read i
parseFactor ts = parseExpr ts

binOp :: Token -> BinOp
binOp (TOp "+") = Add
binOp (TOp "-") = Sub
binOp (TOp "*") = Mul
binOp (TOp "/") = Div
binOp (TOp "%") = Mod
