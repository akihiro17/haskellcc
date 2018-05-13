module Lib
    ( someFunc
    ) where

import Lex

someFunc :: IO ()
someFunc =
  let tokenList = tokens "int main(){int a;\nreturn 2;}" in
    let str = map lexer tokenList in
        mapM_ putStrLn str
