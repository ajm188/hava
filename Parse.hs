module Parse
(
) where

import Lex

data Expr =
    Var String |
    Const Int |
    BinaryExpr Expr BinaryOp Expr
    deriving Show

data BinaryOp = Add | Sub | Mul | Div | Mod
    deriving Show

parseExpr :: [Token] -> Expr
parseExpr ((TInt i):[]) = Const (read i)
parseExpr ((TIdent v):[]) = Var v
parseExpr (x:t@(TOp o):xs)
    | o == "+" || o == "-" = BinaryExpr (parseExpr [x]) (binOp t) (parseTerm xs)

parseTerm :: [Token] -> Expr
parseTerm (x:t@(TOp o):xs)
    | o == "*" || o == "/" || o == "%" = BinaryExpr (parseExpr [x]) (binOp t) (parseExpr xs)
parseTerm x = parseExpr x

binOp :: Token -> BinaryOp
binOp (TOp "+") = Add
binOp (TOp "-") = Sub
binOp (TOp "*") = Mul
binOp (TOp "/") = Div
binOp (TOp "%") = Mod
