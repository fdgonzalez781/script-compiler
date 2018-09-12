module CompilerJS2 (compile) where

-- An alternate JavaScript compiler that compiles all lambda terms to single variable functions.
-- This compiler should be more compatible with the structure of the lambda calculus.

import Compiler (compileSyntax, Program, Statement(..), Term(..), VarDecl(..))
import Data.List
import Control.Monad

test :: String -> IO ()
test str = do
  putStr $ compile str
  input <- getLine
  unless (input == "quit") $ do
    test input
  putStrLn $ "Finished testing JavaScript compiler."

-- Note that the abstract syntax tree is a language-agnostic representation of the input string.
-- As a result, we can use the generated syntax tree as an intermediate representation of the language.
-- This enables us to compile the untyped lambda calculus to any language we choose.

compile :: String -> String
compile = genCode . compileSyntax

genCode :: Program -> String
genCode (st:stmts) = genCodeStmt st ++ "\n" ++ genCode stmts
genCode [] = []

strParams = concat . (intersperse ", ")

genCodeStmt :: Statement -> String
genCodeStmt (Definition name (Abstr (Var x) term)) = "function " ++ name ++ "(" ++ x ++ ") {\n  return "
  ++ genCodeTerm term ++ ";\n}\n"
genCodeStmt (Definition name term) = "function " ++ name ++ "() {\n  return " ++ genCodeTerm term ++ ";\n}\n"
genCodeStmt (LambdaTerm (Abstr (Var x) term)) = "function(" ++ x ++ ") {\n  return " ++ genCodeTerm term ++ ";\n}\n"
genCodeStmt (LambdaTerm term) = "function() {\n  return " ++ genCodeTerm term ++ ";\n}\n"

genCodeTerm :: Term -> String
genCodeTerm term@(Abstr (Var x) t) = "function(" ++ x ++ ") {\n  return " ++ genCodeTerm t ++ ";\n}\n"
genCodeTerm (Apply t1 t2) = genCodeTerm t1 ++ "(" ++ genCodeTerm t2 ++ ")"
genCodeTerm (Call (Var x)) = x
