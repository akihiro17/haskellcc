module Main where

import System.Environment
import Generator
import Parser
import Lex

import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args

  content <- readFile filePath

  case parse Parser.program "" content of
    Left error -> print error
    Right tokenList ->
      let
        assembly = Generator.generate tokenList
      in
        writeFile "program.s" assembly
