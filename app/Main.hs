module Main where

import System.Environment
import Generator
import Parser
import Lex

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args

  program <- readFile filePath

  let tokenList = Lex.tokens program
  let assembly = Generator.generate (Parser.parse tokenList)

  print assembly

  writeFile "program.s" assembly
