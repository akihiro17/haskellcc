module Generator
  (
    generate,
    generateFunction,
    generateStatement,
  ) where

import Data.List
import Ast

generate :: Ast.Program -> String
generate (Ast.Prog decl) =
  let
    assembly = ".globl "
  in
    assembly ++ generateFunction decl

generateFunction :: Ast.FuncDecl -> String
generateFunction (Ast.FuncDecl funcType (Ast.Id funcName) funcParams (Ast.Body statements)) =
  let
    assembly = funcName ++ "\n" ++ funcName ++ ":\n"
    statementsAssembly = map generateStatement statements
  in
    foldl (++) assembly statementsAssembly

generateStatement :: Ast.Statement -> String
generateStatement (Ast.ReturnVal exp) =
  generateExp exp ++ "ret\n"

generateExp :: Ast.Exp -> String
generateExp (Ast.ConstExp(Ast.Int value)) = "movq $" ++ show value ++ ", %rax\n"
generateExp (Ast.UnopExp Ast.Negate exp) =
  let
    expAssembly = generateExp exp
  in
    expAssembly ++ "neg %rax\n"
generateExp (Ast.UnopExp Ast.Complement exp) =
  let
    expAssembly = generateExp exp
  in
    expAssembly ++ "not %rax\n"
generateExp (Ast.UnopExp Ast.Not exp) =
  let
    expAssembly = generateExp exp
  in
    expAssembly ++ "cmpq $0, %rax\nmovq $0, %rax\nsete %al\n"
generateExp (Ast.BinOpExp Ast.Plus left right) =
  let
    leftAssembly = generateExp left
    rightAssembly = generateExp right
  in
    leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "popq %rcx\n" ++ "addq %rcx, %rax\n"
generateExp (Ast.BinOpExp Ast.Minus left right) =
  let
    leftAssembly = generateExp left
    rightAssembly = generateExp right
  in
    leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "popq %rcx\n" ++ "subq %rax, %rcx\nmovq %rcx, %rax"
generateExp (Ast.BinOpExp Ast.Multi left right) =
  let
    leftAssembly = generateExp left
    rightAssembly = generateExp right
  in
    leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "popq %rcx\n" ++ "imulq %rcx, %rax\n"
generateExp (Ast.BinOpExp Ast.Div left right) =
  let
    leftAssembly = generateExp left
    rightAssembly = generateExp right
  in
    leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "movq %rax, %rcx\npopq %rax\nmovq $0, %rdx\nidivq %rcx, %rax\n"
