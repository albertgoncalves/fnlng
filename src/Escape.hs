module Escape (escape) where

import Ast (Expr (..), Func (..), Scope (..), Stmt (..), intrinsics)
import Control.Monad (unless)
import Control.Monad.State (State, evalState, get, gets, modify)
import Data.Bifunctor (first, second)
import qualified Data.Set as S

type LexicalScope = (S.Set String, S.Set String)

captureExpr :: S.Set String -> Expr -> State LexicalScope Expr
captureExpr globals (ExprAccess expr offset) =
  (`ExprAccess` offset) <$> captureExpr globals expr
captureExpr globals (ExprCall func args) =
  ExprCall <$> captureExpr globals func <*> mapM (captureExpr globals) args
captureExpr globals (ExprFunc func) = ExprFunc <$> captureFunc globals func
captureExpr globals expr@(ExprIdent ident) = do
  locals <- gets fst
  unless (S.member ident $ S.union globals locals) $
    modify $
      second $
        S.insert ident
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
  modify $ first $ S.insert ident
  return $ StmtLet ident value1
captureStmt globals (StmtSet target value) =
  StmtSet <$> captureExpr globals target <*> captureExpr globals value
captureStmt globals (StmtVoid expr) = StmtVoid <$> captureExpr globals expr

captureScope :: S.Set String -> Scope -> State LexicalScope Scope
captureScope globals (Scope stmts0 expr0) = do
  locals <- gets fst
  stmts1 <- mapM (captureStmt globals) stmts0
  expr1 <- captureExpr globals expr0
  modify $ first $ const locals
  return $ Scope stmts1 expr1

captureFunc :: S.Set String -> Func -> State LexicalScope Func
captureFunc globals (Func args0 Nothing _ (Scope stmts0 expr0)) = do
  (locals0, captures0) <- get
  let args1 = S.fromList args0
  modify $ const (args1, S.empty)
  stmts1 <- mapM (captureStmt globals) stmts0
  expr1 <- captureExpr globals expr0
  (locals1, captures1) <- get
  let captures2 = S.difference captures1 locals1
  modify $ const (locals0, S.union captures0 $ S.union captures0 captures2)
  return $
    Func
      args0
      (Just $ S.toList captures2)
      (Just $ S.toList $ S.intersection captures1 $ S.difference locals1 args1)
      (Scope stmts1 expr1)
captureFunc _ _ = undefined

boxExpr :: S.Set String -> Expr -> Expr
boxExpr boxed (ExprAccess expr offset) = ExprAccess (boxExpr boxed expr) offset
boxExpr boxed (ExprCall func args) =
  ExprCall (boxExpr boxed func) $ map (boxExpr boxed) args
boxExpr boxed (ExprFunc func) = ExprFunc $ boxFunc boxed func
boxExpr boxed expr@(ExprIdent ident)
  | S.member ident boxed = ExprAccess expr 0
  | otherwise = expr
boxExpr boxed (ExprIfElse expr true false) =
  ExprIfElse (boxExpr boxed expr) (boxScope boxed true) (boxScope boxed false)
boxExpr _ expr = expr

boxStmt :: S.Set String -> Stmt -> Stmt
boxStmt boxed (StmtLet ident value)
  | S.member ident boxed =
      StmtLet ident $ ExprCall ExprBox [boxExpr boxed value]
  | otherwise = StmtLet ident $ boxExpr boxed value
boxStmt boxed (StmtSet target value) =
  StmtSet (boxExpr boxed target) (boxExpr boxed value)
boxStmt boxed (StmtVoid expr) = StmtVoid $ boxExpr boxed expr

boxScope :: S.Set String -> Scope -> Scope
boxScope boxed (Scope stmts expr) =
  Scope (map (boxStmt boxed) stmts) $ boxExpr boxed expr

boxFunc :: S.Set String -> Func -> Func
boxFunc boxed0 (Func args (Just captures) (Just boxed1) scope) =
  Func args (Just captures) (Just boxed1) $
    boxScope
      (S.union (S.fromList boxed1) $ S.difference boxed0 $ S.fromList args)
      scope
boxFunc _ _ = undefined

envExpr :: S.Set String -> Expr -> Expr
envExpr globals (ExprAccess expr offset) =
  ExprAccess (envExpr globals expr) offset
envExpr globals (ExprCall ExprBox args) =
  ExprCall ExprBox $ map (envExpr globals) args
envExpr globals (ExprCall func@(ExprIdent ident) args)
  | S.member ident globals = ExprCall func $ map (envExpr globals) args
envExpr globals (ExprCall func args) =
  ExprCall (closure 0) $ closure 1 : map (envExpr globals) args
  where
    closure = ExprAccess (envExpr globals func)
envExpr globals (ExprFunc func@(Func _ (Just []) _ _)) =
  ExprCall ExprBox [ExprFunc $ envFunc globals func, ExprUndef]
envExpr globals (ExprFunc func@(Func _ (Just captures) _ _)) =
  ExprCall
    ExprBox
    [ ExprFunc $ envFunc globals func,
      ExprCall ExprBox $ map ExprIdent captures
    ]
envExpr _ (ExprFunc _) = undefined
envExpr globals (ExprIfElse expr true false) =
  ExprIfElse
    (envExpr globals expr)
    (envScope globals true)
    (envScope globals false)
envExpr globals expr@(ExprIdent ident)
  | S.member ident globals = ExprCall ExprBox [expr, ExprUndef]
  | otherwise = expr
envExpr _ expr = expr

envStmt :: S.Set String -> Stmt -> Stmt
envStmt globals (StmtLet ident value) = StmtLet ident (envExpr globals value)
envStmt globals (StmtSet target value) =
  StmtSet (envExpr globals target) (envExpr globals value)
envStmt globals (StmtVoid expr) = StmtVoid $ envExpr globals expr

envScope :: S.Set String -> Scope -> Scope
envScope globals (Scope stmts expr) =
  Scope (map (envStmt globals) stmts) $
    envExpr globals expr

envFunc :: S.Set String -> Func -> Func
envFunc globals (Func args (Just []) maybeBoxed scope) =
  Func args (Just []) maybeBoxed $ envScope globals scope
envFunc globals (Func args (Just captures) maybeBoxed scope) =
  Func ("env" : args) (Just captures) maybeBoxed $
    Scope
      ( zipWith
          (\ident -> StmtLet ident . ExprAccess (ExprIdent "env"))
          captures
          [0 ..]
          ++ stmts
      )
      expr
  where
    Scope stmts expr = envScope globals scope
envFunc _ _ = undefined

escape :: [(String, Func)] -> [(String, Func)]
escape labelFuncs =
  zip labels $
    map
      ( envFunc globals
          . boxFunc S.empty
          . (`evalState` (S.empty, S.empty))
          . captureFunc globals
      )
      funcs
  where
    (labels, funcs) = unzip labelFuncs
    globals = S.fromList $ intrinsics ++ labels
