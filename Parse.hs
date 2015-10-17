module Parse
( parseStmts
) where

import Lex
import Utils

data Stmt = AssignStmt Expr BinaryOp Expr | ExprStmt Expr
    deriving Show

data Expr =
    Var String |
    Const Int |
    BinaryExpr Expr BinaryOp Expr
    deriving Show

data BinaryOp = Add | Sub | Mul | Div | Mod | Assign
    deriving Show

data Delim = LParen | RParen | LCurlyBrace | RCurlyBrace
    deriving ( Eq
             , Show)

parseStmts :: [[Token]] -> [Stmt]
parseStmts [] = []
parseStmts (x:xs) = (parseStmt x):(parseStmts xs)

parseStmt :: [Token] -> Stmt
parseStmt (x:[]) = ExprStmt $ parseExpr [x]
parseStmt stmt@(x:(TOp "="):xs) = parseAssignment x xs
parseStmt stmt = ExprStmt $ parseExpr stmt

parseAssignment :: Token -> [Token] -> Stmt
parseAssignment (TIdent var) rhs = AssignStmt (Var var) Assign (parseExpr rhs)

parseExpr :: [Token] -> Expr
-- E -> T | E + E | E - E
parseExpr e =
    case splitFirst [TDelim "("] e of
        Nothing -> f e
        Just([e', p, e'']) -> f' (e'++[TInt "1"]) (p++e'')
    where exprOps = [TOp "+", TOp "-"]
          f e = case splitFirst exprOps e of
                    Nothing -> parseTerm e
                    Just([lhs, [op], rhs]) ->
                        BinaryExpr (parseExpr lhs) (binOp op) (parseExpr rhs)
          f' e rhs = case splitFirst exprOps e of
                        Nothing ->
                            let BinaryExpr e' o' _ = parseTerm e
                            in BinaryExpr e' o' $ parseExpr rhs
                        Just([lhs, [op], rhs']) ->
                            let BinaryExpr e' o' _ = parseExpr lhs
                            in BinaryExpr e' o' $ parseExpr $ rhs' ++ rhs
parseTerm :: [Token] -> Expr
-- T -> F | T * T | T / T | T % T
parseTerm t =
    case splitFirst [TDelim "("] t of
        Nothing -> f t
        Just([t', p, t'']) -> f' (t'++[TInt "1"]) (p++t'')
    where termOps = [TOp "*", TOp "/", TOp "%"]
          f t = case splitFirst termOps t of
                    Nothing -> parseFactor t
                    Just([lhs, [op], rhs]) ->
                        BinaryExpr (parseTerm lhs) (binOp op) (parseTerm rhs)
          f' t rhs = case splitFirst termOps t of
                        Nothing -> 
                            let BinaryExpr t' o' _ = parseFactor t
                            in BinaryExpr t' o' $ parseTerm rhs
                        Just([lhs, [op], rhs']) ->
                            let BinaryExpr t' o' _ = parseTerm lhs
                            in BinaryExpr t' o' $ parseTerm $ rhs' ++ rhs
parseFactor :: [Token] -> Expr
-- F -> Const
parseFactor ((TInt i):[]) = Const $ read i
-- F -> Var
parseFactor ((TIdent var):[]) = Var var
-- F -> ( E )
parseFactor (ld@(TDelim "("):xs) =
    case split of
        Just([body, _, []]) -> parseExpr body
    where split = splitFirst [matchingDelim ld] xs
-- F -> E
parseFactor f = parseExpr f

binOp :: Token -> BinaryOp
binOp (TOp "+") = Add
binOp (TOp "-") = Sub
binOp (TOp "*") = Mul
binOp (TOp "/") = Div
binOp (TOp "%") = Mod
binOp (TOp "=") = Assign

delim :: Token -> Delim
delim (TDelim "(") = LParen
delim (TDelim ")") = RParen
delim (TDelim "{") = LCurlyBrace
delim (TDelim "}") = RCurlyBrace
