module ParserDeBruijn where

import Lexer (TokenType(..), Token(..))

data VarDecl = Var Int deriving (Eq, Show)
data Term = Call VarDecl
          | Abstr VarDecl Term
          | Apply Term Term
          deriving (Eq, Show)

data Statement = LambdaTerm Term
               | Definition String Term
               deriving (Eq, Show)

type Program = [Statement]

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

matchTerm :: [Token] -> Term
matchTerm = undefined

matchTermIndex :: Int -> [Token] -> Term
matchTermIndex n ((Lambda, _):(Ident, x):xs) = Abstr (Var n) (matchTermIndex (n+1) xs)
matchTermIndex n ((Ident, x):(Ident, y):xs) = Apply (Call (Var _)) (Call (Var _))

-- matchStmt takes a list of tokens and parses it into one of two statements:
-- If it begins with a definition clause [ident := ...], then it breaks the definition and parses the body as a term.
-- Otherwise, it is a pure lambda term, and it is parsed as such.

matchStmt :: [Token] -> Statement
matchStmt ((Ident, name):(Defn, _):xs) = Definition name (matchTerm $ pureTerm xs)
matchStmt xs = LambdaTerm $ matchTerm $ pureTerm xs

pureTerm :: [Token] -> [Token]
pureTerm tokens = if ((fst . last) tokens == Endl) then init tokens else tokens
