module Ast (Expr (..), Func (..), Scope (..), Stmt (..), intrinsics) where

import Data.List (intercalate)
import Text.Printf (printf)

data Expr
  = ExprAccess Expr Int
  | ExprBox
  | ExprCall Expr [Expr]
  | ExprFunc Func
  | ExprIdent String
  | ExprIfElse Expr Scope Scope
  | ExprInt Int
  | ExprUndef

data Stmt
  = StmtLet String Expr
  | StmtSet Expr Expr
  | StmtVoid Expr

data Scope = Scope [Stmt] Expr

data Func = Func [String] (Maybe [String]) Scope

instance Show Expr where
  show = showExpr 0

instance Show Stmt where
  show = showStmt 0

instance Show Func where
  show = showFunc 0

instance Show Scope where
  show = showScope 0

indent :: Int -> String
indent = (`replicate` ' ')

showExpr :: Int -> Expr -> String
showExpr n (ExprAccess expr offset) = printf "%s[%d]" (showExpr n expr) offset
showExpr _ ExprBox = "[]"
showExpr n (ExprCall func args) =
  printf "%s(%s)" (showExpr n func) $ intercalate ", " $ map (showExpr n) args
showExpr n (ExprFunc func) = printf "\\%s" $ showFunc n func
showExpr _ (ExprIdent ident) = ident
showExpr n (ExprIfElse expr true false) =
  printf
    "if %s %s else %s"
    (showExpr n expr)
    (showScope n true)
    (showScope n false)
showExpr _ (ExprInt int) = show int
showExpr _ ExprUndef = "_"

showStmt :: Int -> Stmt -> String
showStmt n (StmtLet ident value) = printf "%s := %s" ident $ showExpr n value
showStmt n (StmtSet target value) =
  printf "%s = %s" (showExpr n target) (showExpr n value)
showStmt n (StmtVoid expr) = showExpr n expr

showFunc :: Int -> Func -> String
showFunc n (Func args maybeCaptures scope) =
  printf
    "(%s)%s %s"
    (intercalate ", " args)
    ( case maybeCaptures of
        Just captures -> printf " |%s|" $ intercalate ", " captures
        Nothing -> ""
    )
    $ showScope n scope

showScope :: Int -> Scope -> String
showScope n0 (Scope stmts expr) =
  printf
    "{\n%s%s}"
    ( unlines $
        map (printf "%s%s" $ indent n1) $
          map (printf "%s;" . showStmt n1) stmts ++ [showExpr n1 expr]
    )
    (indent n0)
  where
    n1 = n0 + 4

intrinsics :: [String]
intrinsics = ["print", "+", "-", "=="]
