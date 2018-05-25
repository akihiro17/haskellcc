module Main where

import System.Environment
import Generator
import Parser

import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Control.Monad.State

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args

  content <- readFile filePath

  case parse Parser.program "" content of
    Left error -> print error
    Right tokenList ->
      let
        (assembly, _) = runState (Generator.generate tokenList) (Map.empty, 0)
      in
        writeFile "program.s" assembly
