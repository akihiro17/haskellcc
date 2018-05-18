module GeneratorSpec (spec) where

import Test.Hspec
import Generator
import Parser
import Ast

import Text.ParserCombinators.Parsec

spec :: Spec
spec = do
  describe "parse" $
    it "returns statements" $ do
      let i = parse Parser.program "" "int main(){return 2;}"
      case i of
        Left a -> error "parse error"
        Right tokenList ->
          let
            assembly = Generator.generate tokenList
          in
            assembly `shouldBe` ".globl main\nmain:\nmovq $2, %rax\nret\n"
  describe "parseStatements" $
    it "returns statements" $ do
      let i = parse Parser.statement "" "return 2;"
      case i of
        Left a -> error "parse error"
        Right statement ->
          let
            assembly = Generator.generateStatement statement
          in
            assembly `shouldBe` "movq $2, %rax\nret\n"
  describe "parseStatements" $
    it "neg" $ do
      let i = parse Parser.statement "" "return -2;"
      case i of
        Left a -> error "parse error"
        Right statement ->
          let
            assembly = Generator.generateStatement statement
          in
            assembly `shouldBe` "movq $2, %rax\nneg %rax\nret\n"
  describe "parseStatements" $
    it "complement" $ do
      let i = parse Parser.statement "" "return ~2;"
      case i of
        Left a -> error "parse error"
        Right statement ->
          let
            assembly = Generator.generateStatement statement
          in
            assembly `shouldBe` "movq $2, %rax\nnot %rax\nret\n"
  describe "parseStatements" $
    it "not" $ do
      let i = parse Parser.statement "" "return !2;"
      case i of
        Left a -> error "parse error"
        Right statement ->
          let
            assembly = Generator.generateStatement statement
          in
            assembly `shouldBe` "movq $2, %rax\ncmpq $0, %rax\nmovq $0, %rax\nsete %al\nret\n"
