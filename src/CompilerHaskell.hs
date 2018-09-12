module CompilerHaskell (compile) where

import Compiler (compileSyntax, Program(..), Statement(..), Term(..), VarDecl(..))

compile :: String -> String
compile = genCode . compileSyntax

-- genCode takes a program's syntax tree and generates a valid Haskell string according to its structure.
-- genCode feeds each statement into genCodeStmt, then appends a new line character.

genCode :: Program -> String
genCode [] = []
genCode (x:xs) = genCodeStmt x ++ "\n" ++ genCode xs

-- genCodeStmt translates a statement into a Haskell statement, either as a definition, or by passing it directly
-- to the term code generator, genCodeTerm.

genCodeStmt :: Statement -> String
genCodeStmt (Definition name term) = name ++ " = " ++ genCodeTerm term
genCodeStmt (LambdaTerm term) = genCodeTerm term

-- genCodeTerm takes a lambda term and translates it into a valid Haskell expression.

{- In the untyped lambda calculus, lambda terms will generally translate to functions of arbitrary arity
   with polymorphic types.
   One interesting artifact of representing the untyped lambda calculus in Haskell is the set of conflicts
   that arise as a result of their very different type systems.
   Generally, Haskell's type inference will fill in the gaps regarding the type of a lambda expression,
   however, there do exist well-formed lambda terms that are not well-typed in Haskell.

   To see this in action, try compiling the following statement, which is an instance of the Y combinator:

   fix := lambda f. (lambda x. f (x x)) (lambda x. f (x x));

   fix, as defined above, is an encoding of the Y combinator, a fixed-point combinator that makes
   recursion possible in the untyped lambda calculus.

   If the definition of fix is fed into the compiler, fix will pass all three stages of compilation and parse to
   the following Haskell statement:

   fix = \f -> (\x -> f (x x)) (\x -> f (x x))

   If this definition is then fed into GHCi, the program will fail to load.
   GHCi fails to construct the infinite type of the expression (x x).
   Whereas this expression would be untyped in the lambda calculus, like all other expressions,
   Haskell, being a strongly typed language, attempts to resolve the type of fix immediately.
   As the expression (x x) has no valid type in Haskell's type system, Haskell fails to load the statement into GHCi.

   What is so interesting about this case is that fix passes all stages of compilation into Haskell, and then
   again passes lexical and semantic analysis in the Haskell compiler, only failing incredibly late into compilation:
   at the type inference stage.
-}

genCodeTerm :: Term -> String
genCodeTerm (Call (Var name)) = name
genCodeTerm (Abstr (Var name) term) = "\\" ++ name ++ " -> " ++ genCodeTerm term
genCodeTerm (Apply t1@(Abstr _ _) t2@(Abstr _ _)) = "(" ++ genCodeTerm t1 ++ ") (" ++ genCodeTerm t2 ++ ")"
genCodeTerm (Apply t@(Abstr _ _) term) = "(" ++ genCodeTerm t ++ ") " ++ genCodeTerm term
genCodeTerm (Apply term t@(Abstr _ _)) = genCodeTerm term ++ " (" ++ genCodeTerm t ++ ")"
genCodeTerm (Apply (Call (Var x)) (Call (Var y))) = x ++ " " ++ y
genCodeTerm (Apply (Call (Var x)) t@(Apply t1 t2)) = x ++ " (" ++ genCodeTerm t ++ ")"
genCodeTerm (Apply term (Call (Var x))) = genCodeTerm term ++ " " ++ x
genCodeTerm (Apply t1 t2) = genCodeTerm t1 ++ " (" ++ genCodeTerm t2 ++ ")"
