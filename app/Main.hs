module Main where

import System.Directory

import Lib
import Compiler

main :: IO ()
main = do
  file <- readFileCurrentDir "src/script.txt"
  putStrLn $ compile file

readFileCurrentDir :: String -> IO String
readFileCurrentDir str = getCurrentDirectory >>= \dir -> readFile $ dir ++ "/" ++ str
