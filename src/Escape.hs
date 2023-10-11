module Escape (escape) where

import Ast (Expr (..), Func (..), Scope (..), Stmt (..), intrinsics)
import Control.Monad (unless)
import Control.Monad.State (State, evalState, gets, modify, put)
import qualified Data.Set as S

data LexicalScope = LexicalScope
  { scopeLocals :: S.Set String,
    scopeCaptures :: S.Set String
  }

captureExpr :: S.Set String -> Expr -> State LexicalScope Expr
captureExpr globals (ExprAccess expr offset) =
  (`ExprAccess` offset) <$> captureExpr globals expr
captureExpr globals (ExprCall func args) =
  ExprCall <$> captureExpr globals func <*> mapM (captureExpr globals) args
captureExpr globals (ExprFunc func) = ExprFunc <$> captureFunc globals func
captureExpr globals expr@(ExprIdent ident) = do
  locals <- gets scopeLocals
  unless (S.member ident $ S.union globals locals) $ modify $ \s ->
    s {scopeCaptures = S.insert ident $ scopeCaptures s}
  return expr
captureExpr globals (ExprIfElse expr true false) =
  ExprIfElse
    <$> captureExpr globals expr
    <*> captureScope globals true
    <*> captureScope globals false
captureExpr _ expr = return expr

captureStmt :: S.Set String -> Stmt -> State LexicalScope Stmt
captureStmt globals (StmtLet ident value0) = do
  value1 <- captureExpr globals value0
  modify $ \s -> s {scopeLocals = S.insert ident $ scopeLocals s}
  return $ StmtLet ident value1
captureStmt globals (StmtSet target value) =
  StmtSet <$> captureExpr globals target <*> captureExpr globals value
captureStmt globals (StmtVoid expr) = StmtVoid <$> captureExpr globals expr

captureScope :: S.Set String -> Scope -> State LexicalScope Scope
captureScope globals (Scope stmts0 expr0) = do
  locals <- gets scopeLocals
  stmts1 <- mapM (captureStmt globals) stmts0
  expr1 <- captureExpr globals expr0
  modify $ \s -> s {scopeLocals = locals}
  return $ Scope stmts1 expr1

captureFunc :: S.Set String -> Func -> State LexicalScope Func
captureFunc globals (Func args Nothing (Scope stmts0 expr0)) = do
  locals <- gets scopeLocals
  captures0 <- gets scopeCaptures
  put $ LexicalScope (S.fromList args) S.empty
  stmts1 <- mapM (captureStmt globals) stmts0
  expr1 <- captureExpr globals expr0
  captures1 <- gets scopeCaptures
  put $ LexicalScope locals $ S.union captures0 $ S.difference captures1 locals
  return $ Func args (Just $ S.toList captures1) $ Scope stmts1 expr1
captureFunc _ _ = undefined

escape :: [(String, Func)] -> [(String, Func)]
escape labelFuncs =
  zip labels $
    map
      ((`evalState` LexicalScope S.empty S.empty) . captureFunc globals)
      funcs
  where
    (labels, funcs) = unzip labelFuncs
    globals = S.fromList $ intrinsics ++ labels
