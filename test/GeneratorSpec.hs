module GeneratorSpec (spec) where

import Test.Hspec
import Generator
import Parser
import Lex
import Ast

spec :: Spec
spec = do
  describe "parse" $
    it "returns statements" $ do
      let tokenList = Lex.tokens "int main(){return 2;}"
      let assembly = Generator.generate (Parser.parse tokenList)

      assembly `shouldBe` ".globl main\nmain:\nmovq $2, %rax\nret\n"
  describe "parseStatements" $
    it "returns statements" $ do
      let tokenList = Lex.tokens "return 2;"
      let (statements, _) = Parser.parseStatements tokenList
      let assembly = Generator.generateStatement (head statements)

      assembly `shouldBe` "movq $2, %rax\nret\n"
