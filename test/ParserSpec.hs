module ParserSpec (spec) where

import Test.Hspec
import Parser
import Ast

import Control.Applicative
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Error

spec :: Spec
spec = do
  describe "parse return 23;" $
    it "returns tokens" $ do
      let i = parse Parser.statement "" "return 23;"
      case i of
        Left a -> error "left"
        Right a ->
          let
            Ast.ReturnVal(Ast.ConstExp(Ast.Int i)) = a
          in
            i `shouldBe` 23
  describe "parse return 1+2;" $
    it "returns tokens" $ do
      let i = parse Parser.statement "" "return 1 + 2;"
      case i of
        Left a -> error "left"
        Right a ->
          let
            Ast.ReturnVal(Ast.BinOpExp Ast.Plus left right) = a
          in
            let
              Ast.ConstExp(Ast.Int i) = left
              Ast.ConstExp(Ast.Int j) = right
            in
              [i, j] `shouldMatchList` [1, 2]
  describe "parse return 1+2+3;" $
    it "returns tokens" $ do
      let i = parse Parser.statement "" "return 1+2-3;"
      case i of
        Left a -> error "left"
        Right a ->
          let
            Ast.ReturnVal(Ast.BinOpExp Ast.Minus left right) = a
          in
            let
              Ast.BinOpExp Ast.Plus (Ast.ConstExp(Ast.Int first)) (Ast.ConstExp(Ast.Int second)) = left
              Ast.ConstExp(Ast.Int third) = right
            in
              [first, second, third] `shouldBe` [1, 2, 3]
  describe "parse return 1*2;" $
    it "returns tokens" $ do
      let i = parse Parser.statement "" "return 1 * 2;"
      case i of
        Left a -> error "left"
        Right a ->
          let
            Ast.ReturnVal(Ast.BinOpExp Ast.Multi left right) = a
          in
            let
              Ast.ConstExp(Ast.Int i) = left
              Ast.ConstExp(Ast.Int j) = right
            in
              [i, j] `shouldMatchList` [1, 2]
  describe "parse return -1;" $
    it "returns tokens" $ do
      let i = parse Parser.statement "" "return -1;"
      case i of
        Left a -> error "left"
        Right a ->
          let
            Ast.ReturnVal(Ast.UnopExp Ast.Negate (Ast.ConstExp(Ast.Int value))) = a
          in
            value `shouldBe` 1
  describe "parse program" $
    it "returns tokens" $ do
      let i = parse Parser.program "" "int main() {int a = 2;a= a + 2; return a;}"
      case i of
        Left a -> error "parse errorn"
        Right a -> True
  describe "parse multiple statement" $
    it "returns tokens" $ do
      let i = parse Parser.program "" "int main() {int a = 2;a= a + 3; return a;}"
      case i of
        Left a -> error "error"
        Right a ->
          let
            Ast.Prog(Ast.FuncDecl t main params (Ast.Body stmts)) = a
          in
            let
              Ast.DeclareStatement (Ast.Id id) (Just(Ast.ConstExp(Ast.Int initial))) = head stmts
              ExpStatement (AssignExp (Id lvar) (BinOpExp Plus (VarExp (Id var)) (ConstExp (Int added)))) = stmts !! 1
              Ast.ReturnVal(Ast.VarExp(Ast.Id returnVal)) = stmts !! 2
            in
              [id, show initial, show added, returnVal] `shouldBe` ["a", "2", "3", "a"]
