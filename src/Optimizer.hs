module Optimizer where

-- Consider de Bruijn indexing to simplify structure (and eliminate the need for alpha-conversion).

import Parser (runParser, Program(..), Statement(..), Term(..), VarDecl(..))
import Lexer (runLexer)

test :: String -> Program
test = runParser . runLexer

reduce :: Program -> Program
reduce (st : stmts) = (reduceStmt st) : reduce stmts

reduceStmt :: Statement -> Statement
reduceStmt (Definition name term) = Definition name $ reduceTerm term
reduceStmt (LambdaTerm term) = LambdaTerm $ reduceTerm term

-- Should parse a lambda term and rename any shadowed variable names so that all names in the term are unique.
-- Currently broken.

renameUnique :: Term -> [String] -> Int -> Term
renameUnique t0@(Abstr (Var x) t) xs i =
  if (x `elem` xs) then
    let new = x ++ "_" ++ (show i) in (Abstr (Var new) (renameUnique t (new:xs) (i+1)))
  else Abstr (Var x) (renameUnique t (x:xs) (i+1))
renameUnique t0@(Apply t1 t2) xs i = Apply (renameUnique t1 xs (i+1)) (renameUnique t2 xs (i+1))
renameUnique t0@(Call (Var x)) xs i =
  if (x `elem` xs) then let new = x ++ "_" ++ (show i) in (Call (Var new)) else t0

reduceTerm :: Term -> Term
reduceTerm (Apply (Abstr x t0) t1) = undefined

alpha :: Term -> String -> String -> Term
alpha t@(Call (Var x)) old new =
  if (x == old) then (Call (Var new)) else (if (x == new) then (Call (Var $ new ++ "!")) else t)
alpha t@(Abstr (Var x) term) old new =
  if (x == old) then (Abstr (Var new) (alpha term old new)) else (if (x == new) then (Abstr (Var $ new ++ "!") $ alpha term old new) else t)
alpha t@(Apply t1 t2) old new = Apply (alpha t1 old new) (alpha t2 old new)
