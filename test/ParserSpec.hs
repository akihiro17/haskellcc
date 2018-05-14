module ParserSpec (spec) where

import Test.Hspec
import Parser
import Lex
import Ast

spec :: Spec
spec = do
  describe "parse" $
    it "returns tokens" $ do
      let tokenList = Lex.tokens "int main(){return 1;return 2;}"
      let (Ast.Prog decl) = Parser.parse tokenList
      let (Ast.FuncDecl typeDef id funcparams  (Ast.Body statements)) = decl

      typeDef `shouldBe` Ast.IntType
      id `shouldBe` Ast.Id "main"
      length funcparams `shouldBe` 0
      length statements `shouldBe` 2
  describe "parseStatements" $
    it "returns statements" $ do
      let tokenList = Lex.tokens "return 2;return 1;"
      let (statements, _) = Parser.parseStatements tokenList

      let (Ast.ReturnVal(Ast.ConstExp(Ast.Int value))) = head statements

      value `shouldBe` 2
  describe "parseExp" $
    it "returns exp" $ do
      let tokenList = Lex.tokens "2"

      let (Ast.ConstExp(Ast.Int value), _) = Parser.parseExp tokenList

      value `shouldBe` 2
  describe "unop" $
    it "returns exp" $ do
      let tokenList = Lex.tokens "!~-1"
      let (Ast.UnopExp Ast.Not(Ast.UnopExp Ast.Complement(Ast.UnopExp Ast.Negate(Ast.ConstExp(Ast.Int value)))), _) = Parser.parseExp tokenList

      value `shouldBe` 1
