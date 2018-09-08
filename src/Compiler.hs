module Compiler (compile) where

import Control.Monad
import Text.Regex.PCRE
import Data.Array
import qualified Data.Text as Text
import System.Directory
import Data.List
import Data.Maybe
import System.Eval.Haskell

-- lamdba f. lambda x. f x <=> \f -> \x -> f x
-- lambda f. lamdba x. f x is equivalent to a value of type (a -> b) -> a -> a

-- Examples of valid expressions:
-- x (variable)
-- f x (function application)
-- f (a b) (function application on expr with parens)
-- x0 x1 x2 ... xn (n-ary function application via currying)

-- Grammar definition [expr]:
-- expr ::= ident | ident expr | (expr)

-- Example of valid statements:
-- lambda x. x (bind param 1 to x, return x)
-- lambda f. lambda x. f x (bind param 1 to f, bind param 2 to x, return f applied to x)
-- func := lambda x. x (define function "func" as [bind var x, return x])
-- (lambda f. lambda x. f x) T (bind param 1 to f, bind param 2 to x, return f applied to x, f := T, a lambda term)

-- lambda x. x should parse to \x -> x.
-- lambda f. lambda x. f x should parse to \f -> \x -> f x.
-- (lambda f. lambda x. f x) T should parse to (\f -> \x -> f x) T.
-- func := lambda x. x should parse to
-- OPTIONAL: func :: a -> a
-- REQUIRED: func = \x -> x OR func x = x

data TokenType = Lambda | Ident | Dot | Defn | OParen | CParen | Endl deriving (Eq, Show)
type Token = (TokenType, String)

data VarDecl = Var String deriving (Eq, Show)
data Term = Call VarDecl
          | Abstr VarDecl Term
          | Apply Term Term
          deriving (Eq, Show)

data Statement = LambdaTerm Term
               | Definition String Term
               deriving (Eq, Show)

type Rule = [Token] -> Bool

-- BNF Rules:
-- term ::= ident | lambda ident term | term term
-- statement ::= term | ident ':=' term

type Program = [Statement]

tokenTypes = [
  (Lambda, "\\blambda\\b"),
  (Ident, "\\b[a-zA-Z]+\\b"),
  (Dot, "\\."),
  (Defn, ":="),
  (OParen, "\\("),
  (CParen, "\\)"),
  (Endl, "\\;")
             ]

compile :: String -> String
compile = genCode .
  runParser . runLexer

with :: (Traversable t) => t a -> (a -> b) -> t b
with = flip fmap

strip :: String -> String
strip = Text.unpack . Text.strip . Text.pack

runLexer :: String -> [Token]
runLexer file = concatMap tokenizeLine (lines file)

tokenizeLine :: String -> [Token]
tokenizeLine [] = []
tokenizeLine line =
  let (match, rem) = maybe (error "Error while tokenizing: could not read string") id (tokenizeFirst line)
  in match : tokenizeLine rem

tokenizeFirst :: String -> Maybe (Token, String)
tokenizeFirst [] = Nothing
tokenizeFirst line = head $ filter (/= Nothing) $ with tokenTypes $ \(kind, ptn) ->
  matchOnceText (makeRegex ("\\A" ++ ptn) :: Regex) (strip line) >>= \(_, match, rem) -> Just ((kind, fst $ match ! 0), rem)

breakAfter :: (a -> Bool) -> [a] -> ([a], [a])
breakAfter p xs =
  let (st, end) = break p xs
  in case end of
    [] -> (st, [])
    _ -> (st ++ ([head end]), tail end)

runParser :: [Token] -> Program
runParser tokens =
  let (st, rem) = breakAfter (\token -> fst token == Endl) tokens
  in case rem of
    [] -> [matchStmt st]
    _ -> matchStmt st : runParser rem

matchTerm :: [Token] -> Term
matchTerm ((Lambda, _):(Ident, x):(Dot, _):xs) = Abstr (Var x) (matchTerm xs)
matchTerm ((OParen, x):xs) =
  let (st, rem) = breakAfter (\(token, _) -> token == CParen) xs
  in case rem of
    [] -> matchTerm (init xs)
    ((OParen, _):ys) -> Apply (matchTerm (init st)) (matchTerm (init rem))
    ((Ident, z):[]) -> Apply (matchTerm (init st)) (Call (Var z))
matchTerm ((Ident, x):y:xs) = case y of
  (Lambda, _) -> Apply (Call (Var x)) (matchTerm (y:xs))
  (Ident, _) -> Apply (Call (Var x)) (matchTerm (y:xs))
  (OParen, _) -> Apply (Call (Var x)) (matchTerm (y:(takeWhile (\(token, _) -> token /= CParen) xs)))
  _ -> Call (Var x)
matchTerm ((Ident, x):[]) = Call (Var x)

matchStmt :: [Token] -> Statement
matchStmt ((Ident, name):(Defn, _):xs) = Definition name (matchTerm xs)
matchStmt xs = LambdaTerm $ matchTerm xs

genCode :: Program -> String
genCode [] = []
genCode (x:xs) = genCodeStmt x ++ "\n" ++ genCode xs

genCodeStmt :: Statement -> String
genCodeStmt (Definition name term) = name ++ " = " ++ genCodeTerm term
genCodeStmt (LambdaTerm term) = genCodeTerm term

genCodeTerm :: Term -> String
genCodeTerm (Call (Var name)) = name
genCodeTerm (Abstr (Var name) term) = "\\" ++ name ++ " -> " ++ genCodeTerm term
genCodeTerm (Apply (Call (Var x)) term) = x ++ " " ++ genCodeTerm term
genCodeTerm (Apply term (Call (Var x))) = "(" ++ genCodeTerm term ++ ") " ++ x
genCodeTerm (Apply t1 t2) = "(" ++ genCodeTerm t1 ++ ") (" ++ genCodeTerm t2 ++ ")"
