{-# LANGUAGE LambdaCase #-}

module Parse (parse) where

import Ast (Expr (..), Func (..), Scope (..), Stmt (..))
import Data.Char (isAlpha, isDigit, isSpace)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    get,
    look,
    many,
    many1,
    munch,
    munch1,
    pfail,
    readP_to_S,
    satisfy,
    sepBy,
    string,
    (<++),
  )

choice :: [ReadP a] -> ReadP a
choice [] = pfail
choice [p] = p
choice (p : ps) = p <++ choice ps

space :: ReadP ()
space =
  look >>= \case
    ('#' : _) -> get *> munch (/= '\n') *> ((char '\n' *> space) <++ eof)
    (c : _) | isSpace c -> get *> space
    _ -> return ()

token :: ReadP a -> ReadP a
token = (space *>)

tokenChar :: Char -> ReadP Char
tokenChar = token . char

tokenString :: String -> ReadP String
tokenString = token . string

parens :: ReadP a -> ReadP a
parens p = tokenChar '(' *> anyParens p <* tokenChar ')'

anyParens :: ReadP a -> ReadP a
anyParens p = p <++ parens p

ident :: ReadP String
ident = token $ (:) <$> satisfy isAlpha <*> munch (\x -> isAlpha x || x == '_')

operator :: ReadP String
operator = token $ munch1 (`elem` "=+-")

unsignedInt :: ReadP Int
unsignedInt = read <$> token (munch1 isDigit)

signedInt :: ReadP Int
signedInt = read <$> ((:) <$> tokenChar '-' <*> munch1 isDigit)

exprCallArgs :: ReadP (Either Int [Expr])
exprCallArgs =
  (Right <$> (tokenChar '(' *> (expr `sepBy` tokenChar ',') <* tokenChar ')'))
    <++ (Left <$> (tokenChar '[' *> unsignedInt <* tokenChar ']'))

exprCall :: ReadP Expr
exprCall =
  foldl (flip $ either (flip ExprAccess) (flip ExprCall))
    <$> exprAtom
    <*> many1 exprCallArgs

exprIfElse :: ReadP Expr
exprIfElse =
  ExprIfElse
    <$> (tokenString "if" *> expr)
    <*> scope
    <*> (tokenString "else" *> scope)

exprAtom :: ReadP Expr
exprAtom =
  choice
    [ ExprUndef <$ tokenChar '_',
      ExprInt <$> (signedInt <++ unsignedInt),
      exprIfElse,
      ExprIdent <$> (ident <++ operator),
      ExprBox <$ tokenChar '[' <* tokenChar ']',
      ExprFunc <$> (tokenChar '\\' *> func),
      parens expr
    ]

expr :: ReadP Expr
expr = exprCall <++ exprAtom

stmt :: ReadP Stmt
stmt =
  choice
    [ StmtLet <$> (ident <* tokenString ":=") <*> expr,
      StmtSet <$> (expr <* tokenChar '=') <*> expr,
      StmtVoid <$> expr
    ]

scope :: ReadP Scope
scope =
  Scope
    <$> (tokenChar '{' *> many (stmt <* tokenChar ';'))
    <*> (expr <* tokenChar '}')

func :: ReadP Func
func =
  Func
    <$> (tokenChar '(' *> (ident `sepBy` tokenChar ',') <* tokenChar ')')
    <*> pure Nothing
    <*> scope

parse :: String -> [(String, Func)]
parse = fst . head . readP_to_S (many1 ((,) <$> ident <*> func) <* token eof)
