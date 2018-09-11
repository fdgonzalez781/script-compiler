module JSInterface (compileJS) where

import Compiler (compileSyntax, Program, Statement(..), Term(..), VarDecl(..))
import Data.List

-- Note that the abstract syntax tree is a language-agnostic representation of the input string.
-- As a result, we can use the generated syntax tree as an intermediate representation of the language.
-- This enables us to compile the untyped lambda calculus to any language we choose.

testCompileJS :: String -> IO ()
testCompileJS str = putStr $ compileJS str

compileJS :: String -> String
compileJS = genCode . compileSyntax

genCode :: Program -> String
genCode (st:stmts) = genCodeStmt st ++ "\n" ++ genCode stmts
genCode [] = []

strParams = concat . (intersperse ", ")

genCodeStmt :: Statement -> String
genCodeStmt (Definition name term) =
  let (params, t) = splitParams term
  in "function " ++ name ++ "(" ++ (strParams params) ++ ") {\n  return " ++ genCodeTerm t
  ++ ";\n}\n"
genCodeStmt (LambdaTerm term) =
  let (params, t) = splitParams term
  in "function(" ++ (strParams params) ++ ") {\n  return " ++ genCodeTerm t ++ ";\n}\n"

genCodeTerm :: Term -> String
genCodeTerm term@(Abstr _ _) =
  let (params, t) = splitParams term
  in "function(" ++ (strParams params) ++ ") {\n  return " ++ genCodeTerm t ++ ";\n}\n"
genCodeTerm (Apply t1 t2) = genCodeTerm t1 ++ "(" ++ genCodeTerm t2 ++ ")"
genCodeTerm (Call (Var x)) = x

splitParams :: Term -> ([String], Term)
splitParams t@(Call _) = ([], t)
splitParams t@(Apply _ _) = ([], t)
splitParams (Abstr (Var x) term) = let (names, t) = splitParams term in (x : names, t)
