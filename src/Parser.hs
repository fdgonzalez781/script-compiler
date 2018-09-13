module Parser (runParser, Program(..), Statement(..), Term(..), VarDecl(..)) where

import Lexer (TokenType(..), Token(..))

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
