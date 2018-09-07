module Compiler where

import Control.Monad
import Text.Regex.PCRE
import Data.Array
import qualified Data.Text as Text
import System.Directory

data TokenType = Lambda | Var | Ident | Dot | Definition | OParen | CParen deriving (Eq, Show)
type Token = (TokenType, String)

data Program = Program Statement Program | EndProgram deriving (Eq, Show)

data Statement = BindVar { name :: String }
               | BindIdent { name :: String }
               | Apply { function :: String, arg :: String }
               deriving (Eq, Show)

tokenTypes = [
  (Lambda, "\\blambda\\b"),
  (Ident, "\\b[a-zA-Z]+\\b"),
  (Dot, "\\."),
  (Definition, ":="),
  (OParen, "\\("),
  (CParen, "\\)")
             ]

readFileCurrentDir :: String -> IO String
readFileCurrentDir str = getCurrentDirectory >>= \dir -> readFile $ dir ++ "/" ++ str

compile :: String -> String
compile = genCode . runParser . runLexer

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

runParser :: [Token] -> Program
runParser ((Lambda, _):(Ident, name):(Dot, _):xs) = Program (BindVar name) (runParser xs)
runParser ((Ident, f):(Ident, x):xs) = Program (Apply f x) (runParser xs)
runParser _ = EndProgram

genCode :: Program -> String
genCode program = undefined

testLexer :: IO ()
testLexer = do
  file <- readFileCurrentDir "src/script.txt"
  print $ map fst $ runLexer file
