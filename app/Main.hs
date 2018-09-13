module Main where

import System.Directory
import Control.Monad

import Lib
import qualified CompilerHaskell as Haskell
import qualified CompilerJS as JS
import qualified CompilerJS2 as JS2
import qualified Compiler as Cmp

repl :: IO ()
repl = getLine >>= \input -> unless (input == "quit") $
  let process = putStr . compileLambda . reduce . Cmp.compileSyntax
  in process input >> repl

-- Should perform beta reduction on the program until all terms are in beta normal form.

reduce :: Cmp.Program -> Cmp.Program
reduce prog = undefined

-- Should recompile a lambda program syntax tree back into the source language.

compileLambda :: Cmp.Program -> String
compileLambda prog = undefined

main :: IO ()
main = do
  putStr $ "Which language would you like to compile to? 1. Haskell, 2. JavaScript, 3. JavaScript (alternate), quit. Quit: "
  input <- getLine
  unless (input == "quit") $ do
    code <- getLine
    case (read input :: Int) of
      1 -> compilerLoop (Haskell.compile) code
      2 -> compilerLoop (JS.compile) code
      3 -> compilerLoop (JS2.compile) code
      otherwise -> putStrLn "Invalid input. Please input a number from 1 to 3 to select a language."
    main

compilerLoop :: (String -> String) -> String -> IO ()
compilerLoop comp code = do
  putStr $ comp code
  putStr "Please input an expression to be compiled, or quit to return to main menu: "
  input <- getLine
  unless (input == "quit") $ do
    compilerLoop comp input
  main

readFileCurrentDir :: String -> IO String
readFileCurrentDir str = getCurrentDirectory >>= \dir -> readFile $ dir ++ "/" ++ str

writeFileCurrentDir :: String -> String -> IO ()
writeFileCurrentDir file contents = getCurrentDirectory >>= \dir -> writeFile (dir ++ "/" ++ file) contents

testCompile :: IO ()
testCompile = do
  file <- readFileCurrentDir "src/script.txt"
  writeFileCurrentDir "src/script.hs" (Haskell.compile file)

testCompileJS :: IO ()
testCompileJS = do
  file <- readFileCurrentDir "src/script.txt"
  writeFileCurrentDir "src/script.js" (JS.compile file)

testCompileJS2 :: IO ()
testCompileJS2 = do
  file <- readFileCurrentDir "src/script.txt"
  writeFileCurrentDir "src/script2.js" (JS2.compile file)
