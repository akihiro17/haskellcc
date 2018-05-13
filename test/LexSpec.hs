module LexSpec (spec) where

import Test.Hspec
import Lex

spec :: Spec
spec =
  describe "tokens" $
    it "returns tokens" $ do
      let tokenList = tokens "int main(){int a;\nreturn 2;}"
      let strList = map lexer tokenList
      strList `shouldMatchList` ["int", "main", "(", ")", "{", "int", "a", ";", "return", "2", ";", "}"]