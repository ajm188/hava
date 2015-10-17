module Parse
(
) where

import Control.Applicative hiding (Const)
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

data Delim = LParen | RParen | LCurlyBrace | RCurlyBrace
    deriving ( Eq
             , Show)

parseStmts :: [[Token]] -> [Stmt]
parseStmts [] = []
parseStmts (x:xs) = (parseStmt x):(parseStmts xs)

parseStmt :: [Token] -> Stmt
parseStmt (x:[]) = ExprStmt $ parseExpr [x]
parseStmt stmt@(t@(TOp o):xs)
    | o == "(" = ExprStmt $ parseExpr stmt
parseStmt stmt@(x:t@(TOp o):xs)
    | o == "+" || o == "-" = ExprStmt $ parseExpr stmt
    | o == "*" || o == "/" || o == "%" = ExprStmt $ parseTerm stmt
    | o == "=" = parseAssignment x xs

parseAssignment :: Token -> [Token] -> Stmt
parseAssignment x@(TIdent _) (rhs:[]) = AssignStmt (parseVar x) Assign (parseExpr [rhs])
parseAssignment x@(TIdent _) rhs@(ld@(TDelim d):rem)
    | d == "(" = AssignStmt (parseVar x) Assign (parseExpr rhs)
parseAssignment x@(TIdent _) rhs@(x':(TOp o):xs) = AssignStmt (parseVar x) Assign rhs'
    where rhs' = case o of "+" -> parseExpr rhs
                           "-" -> parseExpr rhs
                           "*" -> parseTerm rhs
                           "/" -> parseTerm rhs
                           "%" -> parseTerm rhs

parseExpr :: [Token] -> Expr
parseExpr ((TInt i):[]) = Const (read i)
parseExpr (var@(TIdent _):[]) = parseVar var
parseExpr (ld@(TDelim d):xs)
    | d == "(" = case body of
                    Just(e) -> case rem of
                                Nothing -> parseExpr e
                                Just([]) -> parseExpr e
                                Just((t@(TOp o):ts)) -> BinaryExpr (parseExpr e) (binOp t) (parseExpr ts)
    where target = matchingDelim $ delim ld
          body = beforeDelim target xs
          rem = afterDelim target xs
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

delim :: Token -> Delim
delim (TDelim "(") = LParen
delim (TDelim ")") = RParen
delim (TDelim "{") = LCurlyBrace
delim (TDelim "}") = RCurlyBrace

matchingDelim :: Delim -> Delim
matchingDelim LParen = RParen
matchingDelim LCurlyBrace = RCurlyBrace

beforeDelim :: Delim -> [Token] -> Maybe [Token]
beforeDelim target [] = Nothing
beforeDelim target (x@(TDelim _):xs)
    | target == delim x = Just []
    | otherwise = Nothing
beforeDelim target (x:xs) = Just(\l -> x:l) <*> beforeDelim target xs

afterDelim :: Delim -> [Token] -> Maybe [Token]
afterDelim target [] = Nothing
afterDelim target (x@(TDelim _):xs)
    | target == delim x = Just xs
    | otherwise = afterDelim target xs
afterDelim target (x:xs) = afterDelim target xs
