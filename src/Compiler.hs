module Compiler (compileSyntax, Program, Statement(..), Term(..), VarDecl(..)) where

import Control.Monad
import Text.Regex.PCRE
import Data.Array
import qualified Data.Text as Text
import System.Directory
import Data.List
import Data.Maybe

-- lamdba f. lambda x. f x <=> \f -> \x -> f x
-- lambda f. lamdba x. f x is equivalent to a value of type (a -> b) -> a -> b

{- Examples of valid expressions:
   x (variable)
   f x (function application)
   f (a b) (function application on expr with parens)
   x0 x1 x2 ... xn (n-ary function application via currying)
-}

-- Grammar definition [expr]:
-- expr ::= ident | ident expr | (expr)

{- Example of valid statements:
   lambda x. x (bind param 1 to x, return x)
   lambda f. lambda x. f x (bind param 1 to f, bind param 2 to x, return f applied to x)
   func := lambda x. x (define function "func" as [bind var x, return x])
   (lambda f. lambda x. f x) T (bind param 1 to f, bind param 2 to x, return f applied to x, f := T, a lambda term)
-}

{- lambda x. x should parse to \x -> x.
   lambda f. lambda x. f x should parse to \f -> \x -> f x.
   (lambda f. lambda x. f x) T should parse to (\f -> \x -> f x) T.
   func := lambda x. x should parse to
   OPTIONAL: func :: a -> a
   REQUIRED: func = \x -> x OR func x = x
-}

-- Due to the simplicity of algebraic data types, we can encode tokens as terms of a single sum type.

data TokenType = Lambda | Ident | Dot | Defn | OParen | CParen | Endl deriving (Eq, Show)
type Token = (TokenType, String)

-- Likewise, the rules of the grammar can be encoded using algebraic data types,
-- and we get the added benefit of being able to encode the recursive structure of the grammar
-- directly into the types.

data VarDecl = Var String deriving (Eq, Show)
data Term = Call VarDecl
          | Abstr VarDecl Term
          | Apply Term Term
          deriving (Eq, Show)

{- As the lambda calculus has a very concise grammar, there are only three different kinds of terms.

   In fact, the lambda calculus does not normally have definition statements. I added them to the grammar
   to allow me to interact with the output code through GHCi.

   Otherwise, lambda terms would be the only valid statements in the language,
   and all lambda terms would be unbound, making it difficult to interact with the program unless
   terms were manually bound in the compiler (which is not a design decision I would like to make).
-}

data Statement = LambdaTerm Term
               | Definition String Term
               deriving (Eq, Show)

-- BNF Rules:
-- term ::= ident | lambda ident '.' term | term term
-- statement ::= term | ident ':=' term

type Program = [Statement]

-- We define a list of pairs of type (TokenType, String), where the second element is the regular expression that corresponds
-- to the token type in the language. This is so that the parser can match subexpressions of the input string
-- with valid tokens in the language.

tokenTypes = [
  (Lambda, "\\blambda\\b"),
  (Ident, "\\b[a-zA-Z]+\\b"),
  (Dot, "\\."),
  (Defn, ":="),
  (OParen, "\\("),
  (CParen, "\\)"),
  (Endl, "\\;")
             ]

-- In order to support compilation to other languages, I've added a function that returns
-- the generated syntax tree, rather than a string of Haskell code.

compileSyntax :: String -> Program
compileSyntax = runParser . runLexer

-- with is a utility function, which I have defined here just for simplicity (see tokenizeFirst).

with :: (Traversable t) => t a -> (a -> b) -> t b
with = flip fmap

-- strip is another utility function that removes all leading and ending whitespace from a string.
-- As the language does not have significant whitespace, it is valid to strip the whitespace,
-- which allows for an easier time tokenizing the input string.

strip :: String -> String
strip = Text.unpack . Text.strip . Text.pack

-- runLexer tokenizes each line of the program through tokenizeLine, then concatenates all of the tokens together.

runLexer :: String -> [Token]
runLexer file = concatMap tokenizeLine (lines file)

-- tokenizeLine grabs the first token in a string using tokenizeFirst, appends it to the front of the token list,
-- then passes the rest of the string back into tokenizeLine.
-- If no match is found in a string, then there is a lexical error.
-- As tokenizeFirst returns a Maybe (Token, String), matches are represented by Just (?) values, while
-- no match found is represented by the value Nothing.

tokenizeLine :: String -> [Token]
tokenizeLine [] = []
tokenizeLine line =
  let (match, rem) = maybe (error "Error while tokenizing: could not read string") id (tokenizeFirst line)
  in match : tokenizeLine rem

-- tokenizeFirst returns a term of type Maybe (Token, String), which can have either the form
-- Just (token, rest), where token is the token found in the string, and rest is the remainder of the input, or
-- Nothing, if there is no match.

tokenizeFirst :: String -> Maybe (Token, String)
tokenizeFirst [] = Nothing
tokenizeFirst line = listToMaybe . catMaybes . with tokenTypes $ \(kind, ptn) ->
  matchOnceText (makeRegex ("\\A" ++ ptn) :: Regex) (strip line) >>= \(_, match, rem) -> Just ((kind, fst $ match ! 0), rem)

-- breakAfter splits a list into a pair of lists, where the first contains all elements up to and including
-- the first element to pass the predicate, and the second contains the rest of the list.
-- This is mainly a utility function used to split a list after the tokens Endl or CParen (end line, closed paren).

breakAfter :: (a -> Bool) -> [a] -> ([a], [a])
breakAfter p xs =
  let (st, end) = break p xs
  in case end of
    [] -> (st, [])
    _ -> (st ++ ([head end]), tail end)

-- findScope takes a starting scope and a list of tokens, and returns a list of tuples, where
-- the first element is the scope level of the token, and the second element is the relevant token.

findScope :: Int -> [Token] -> [(Int, Token)]
findScope s ((OParen, p):xs) = (s+1, (OParen, p)) : findScope (s+1) xs
findScope s ((CParen, p):xs) = (s, (CParen, p)) : findScope (s-1) xs
findScope s (x:xs) = (s, x) : findScope s xs
findScope _ [] = []

-- breakScopeSt takes a scope-annotated list of tokens and splits on the first change in scope.
-- In this case, breakScopeSt will split after the first CParen token of equal scope.

breakScopeSt :: [(Int, Token)] -> ([Token], [Token])
breakScopeSt l@((0, k):xs) = (map snd l, [])
breakScopeSt l@((n, k):xs) = (\(a, b) -> (init . tail $ map snd a, map snd b)) $ breakAfter (\(i, (token, _)) -> i == n && token == CParen) l
breakScopeSt [] = ([], [])

-- breakScope splits a list of tokens after the scope of the first token ends.

breakScope :: [Token] -> ([Token], [Token])
breakScope = breakScopeSt . (findScope 0)

-- runParser takes a list of tokens and parses them line by line using matchStmt.

runParser :: [Token] -> Program
runParser tokens =
  let (st, rem) = breakAfter (\token -> fst token == Endl) tokens
  in case rem of
    [] -> [matchStmt st]
    _ -> matchStmt st : runParser rem

-- matchTerm takes a list of tokens and parses it into the corresponding lambda term syntax tree.

{-
matchTerm :: [Token] -> Term
matchTerm ((Lambda, _):(Ident, x):(Dot, _):xs) = Abstr (Var x) (matchTerm xs)
matchTerm ((OParen, x):xs) =
  let (st, rem) = breakScope ((OParen, x):xs)
  in case rem of
    [] -> matchTerm (init xs)
    ((OParen, _):ys) -> Apply (matchTerm st) (matchTerm rem)
    ((Ident, z):[]) -> Apply (matchTerm st) (Call (Var z))
matchTerm ((Ident, x):y:xs) = case y of
  (Lambda, _) -> Apply (Call (Var x)) (matchTerm (y:xs))
  (Ident, z) -> case xs of
    [] -> Apply (Call (Var x)) (Call (Var z))
    ((OParen, _):ts) -> Apply (Apply (Call (Var x)) (Call (Var z))) (matchTerm xs)
    (t:ts) -> Apply (Apply (Apply (Call (Var x)) (Call (Var z))) (matchTerm [t])) (matchTerm ts)
  (OParen, _) -> Apply (Call (Var x)) (matchTerm (fst $ breakScope (y:xs)))
  _ -> Call (Var x)
matchTerm ((Ident, x):[]) = Call (Var x)
-}

matchTerm :: [Token] -> Term
matchTerm = buildApply Nothing

buildApply :: Maybe Term -> [Token] -> Term
buildApply (Just acc) ts@(x:xs) = case x of
  (Lambda, _) -> Abstr (Var $ snd $ head xs) (buildApply Nothing (tail $ tail xs))
  (Ident, t) -> buildApply (Just (Apply acc (Call (Var t)))) xs
  (OParen, _) ->
    let (st, rem) = breakScope ts
    in case rem of
      [] -> Apply acc (buildApply Nothing st)
      _ -> buildApply (Just (Apply acc (buildApply Nothing st))) rem
  (CParen, _) -> error "Found closing parenthesis without matching open parenthesis"
buildApply Nothing ts@(x:xs) = case x of
  (Lambda, _) -> Abstr (Var $ snd $ head xs) (buildApply Nothing (tail $ tail xs))
  (Ident, t) -> buildApply (Just (Call (Var t))) xs
  (OParen, _) ->
    let (st, rem) = breakScope ts
    in case rem of
      [] -> buildApply Nothing st
      _ -> buildApply (Just (buildApply Nothing st)) rem
  (CParen, _) -> error "Found closing parenthesis without matching open parenthesis"
buildApply (Just acc) [] = acc
buildApply Nothing [] = error "Could not parse empty term"

-- matchStmt takes a list of tokens and parses it into one of two statements:
-- If it begins with a definition clause [ident := ...], then it breaks the definition and parses the body as a term.
-- Otherwise, it is a pure lambda term, and it is parsed as such.

matchStmt :: [Token] -> Statement
matchStmt ((Ident, name):(Defn, _):xs) = Definition name (matchTerm $ pureTerm xs)
matchStmt xs = LambdaTerm $ matchTerm $ pureTerm xs

pureTerm :: [Token] -> [Token]
pureTerm tokens = if ((fst . last) tokens == Endl) then init tokens else tokens
