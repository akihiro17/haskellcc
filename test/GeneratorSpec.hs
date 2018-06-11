module GeneratorSpec (spec) where

import Test.Hspec
import Generator
import Parser
import Ast

import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Control.Monad.State

spec :: Spec
spec = do
  describe "parse" $
    it "returns statements" $ do
      let i = parse Parser.program "" "int main(){return 2;}"
      case i of
        Left a -> error "parse error"
        Right tokenList ->
          let
            (assembly, _) = runState (Generator.generate tokenList) (Map.empty, 0, "", "")
          in
            assembly `shouldBe` ".globl main\nmain:\npushq %rbp\nmovq %rsp, %rbp\nmovq $2, %rax\nmovq %rbp, %rsp\npopq %rbp\nret\n"
  describe "parseStatements" $
    it "returns statements" $ do
      let i = parse Parser.statement "" "return 2;"
      case i of
        Left a -> error "parse error"
        Right statement ->
          let
            (assembly, _) = runState (Generator.generateBlockItems [Ast.StatementItem statement]) (Map.empty, 0, "", "")
          in
            assembly `shouldBe` "movq $2, %rax\nmovq %rbp, %rsp\npopq %rbp\nret\n"
  describe "parseStatements" $
    it "neg" $ do
      let i = parse Parser.statement "" "return -2;"
      case i of
        Left a -> error "parse error"
        Right statement ->
          let
            (assembly, _) = runState (Generator.generateBlockItems [Ast.StatementItem statement]) (Map.empty, 0, "", "")
          in
            assembly `shouldBe` "movq $2, %rax\nneg %rax\nmovq %rbp, %rsp\npopq %rbp\nret\n"
  describe "parseStatements" $
    it "complement" $ do
      let i = parse Parser.statement "" "return ~2;"
      case i of
        Left a -> error "parse error"
        Right statement ->
          let
            (assembly, _) = runState (Generator.generateBlockItems [Ast.StatementItem statement]) (Map.empty, 0, "", "")
          in
            assembly `shouldBe` "movq $2, %rax\nnot %rax\nmovq %rbp, %rsp\npopq %rbp\nret\n"
  describe "parseStatements" $
    it "not" $ do
      let i = parse Parser.statement "" "return !2;"
      case i of
        Left a -> error "parse error"
        Right statement ->
          let
            (assembly, _) = runState (Generator.generateBlockItems [Ast.StatementItem statement]) (Map.empty, 0, "", "")
          in
            assembly `shouldBe` "movq $2, %rax\ncmpq $0, %rax\nmovq $0, %rax\nsete %al\nmovq %rbp, %rsp\npopq %rbp\nret\n"
  describe "parseStatements" $
    it "declare" $ do
      let i = parse Parser.blockItem "" "int a;"
      case i of
        Left a -> error "parse error"
        Right blockItem ->
          let
            (assembly, _) = runState (Generator.generateBlockItems [blockItem]) (Map.empty, 0, "", "")
          in
            assembly `shouldBe` "movq $0, %rax\npushq %rax\n"
