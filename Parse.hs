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
-- E -> ( E )
parseExpr (ld@(TDelim "("):xs) =
    case split of
        Just([body, _, []]) -> parseExpr body
        Just([body, _, rest]) -> parseExpr body -- TODO
    where split = splitFirst [matchingDelim ld] xs
-- E -> T | E + E | E - E
parseExpr e =
    case split of
        Nothing -> parseTerm e
        Just([lhs, [op], rhs]) -> BinaryExpr (parseExpr lhs) (binOp op) (parseExpr rhs)
    where split = splitFirst [TOp "+", TOp "-"] e

parseTerm :: [Token] -> Expr
-- T -> F | T * T | T / T | T % T
parseTerm t =
    case split of
        Nothing -> parseFactor t
        Just([lhs, [op], rhs]) -> BinaryExpr (parseTerm lhs) (binOp op) (parseTerm rhs)
    where split = splitFirst [TOp "*", TOp "/", TOp "%"] t

parseFactor :: [Token] -> Expr
-- F -> Const
parseFactor ((TInt i):[]) = Const $ read i
-- F -> Var
parseFactor ((TIdent var):[]) = Var var
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
