module Parse
(
) where

import Lex

data Stmt = AssignStmt Expr BinaryOp Expr | ExprStmt Expr
    deriving Show

data Expr =
    Var String |
    Const Int |
    BinaryExpr Expr BinaryOp Expr
    deriving Show

data BinaryOp = Add | Sub | Mul | Div | Mod | Assign
    deriving Show

parseStmts :: [[Token]] -> [Stmt]
parseStmts [] = []
parseStmts (x:xs) = (parseStmt x):(parseStmts xs)

parseStmt :: [Token] -> Stmt
parseStmt (x:[]) = ExprStmt $ parseExpr [x]
parseStmt stmt@(x:t@(TOp o):xs)
    | o == "+" || o == "-" = ExprStmt $ parseExpr stmt
    | o == "*" || o == "/" || o == "%" = ExprStmt $ parseTerm stmt
    | o == "=" = parseAssignment x xs

parseAssignment :: Token -> [Token] -> Stmt
parseAssignment x@(TIdent _) (x':[]) = AssignStmt (parseVar x) Assign (parseExpr [x'])
parseAssignment x@(TIdent _) rem@(x':(TOp o):xs) = AssignStmt (parseVar x) Assign rhs
    where rhs = case o of "+" -> parseExpr rem
                          "-" -> parseExpr rem
                          "*" -> parseTerm rem
                          "/" -> parseTerm rem
                          "%" -> parseTerm rem

parseExpr :: [Token] -> Expr
parseExpr ((TInt i):[]) = Const (read i)
parseExpr (var@(TIdent _):[]) = parseVar var
parseExpr (x:t@(TOp o):xs)
    | o == "+" || o == "-" = BinaryExpr (parseExpr [x]) (binOp t) (parseTerm xs)

parseTerm :: [Token] -> Expr
parseTerm (x:t@(TOp o):xs)
    | o == "*" || o == "/" || o == "%" = BinaryExpr (parseExpr [x]) (binOp t) (parseExpr xs)
parseTerm x = parseExpr x

parseVar :: Token -> Expr
parseVar (TIdent v) = Var v

binOp :: Token -> BinaryOp
binOp (TOp "+") = Add
binOp (TOp "-") = Sub
binOp (TOp "*") = Mul
binOp (TOp "/") = Div
binOp (TOp "%") = Mod
binOp (TOp "=") = Assign