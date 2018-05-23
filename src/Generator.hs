module Generator
  (
    generate,
    generateFunction,
    generateStatement,
    generateStatements,
  ) where

import Data.List
import Ast
import qualified Data.Map as Map

data VarMap = Map String Int

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
    statementsAssembly = generateStatements statements Map.empty
  in
    assembly ++ statementsAssembly

generateStatements :: [Ast.Statement] -> Map.Map String Int -> String
generateStatements (stmt:rest) varMap =
  let
    (asm, varMap1) = generateStatement stmt varMap
  in
    asm ++ generateStatements rest varMap1
generateStatements [] varMap = ""

-- <statement> ::= "return" <exp> ";" | <exp> ";" | "int" <id> [ = <exp>] ";"
generateStatement :: Ast.Statement -> Map.Map String Int -> (String, Map.Map String Int)
generateStatement (Ast.ReturnVal exp) varMap =
  (generateExp exp varMap ++ "ret\n", varMap)
generateStatement (Ast.ExpStatement exp) varMap =
  (generateExp exp varMap ++ "\n", varMap)

generateExp :: Ast.Exp -> Map.Map String Int -> String
generateExp (Ast.ConstExp(Ast.Int value)) varMap = "movq $" ++ show value ++ ", %rax\n"
generateExp (Ast.UnopExp Ast.Negate exp) varMap =
  let
    expAssembly = generateExp exp varMap
  in
    expAssembly ++ "neg %rax\n"
generateExp (Ast.UnopExp Ast.Complement exp) varMap =
  let
    expAssembly = generateExp exp varMap
  in
    expAssembly ++ "not %rax\n"
generateExp (Ast.UnopExp Ast.Not exp) varMap =
  let
    expAssembly = generateExp exp varMap
  in
    expAssembly ++ "cmpq $0, %rax\nmovq $0, %rax\nsete %al\n"
generateExp (Ast.BinOpExp Ast.Plus left right) varMap =
  let
    leftAssembly = generateExp left varMap
    rightAssembly = generateExp right varMap
  in
    leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "popq %rcx\n" ++ "addq %rcx, %rax\n"
generateExp (Ast.BinOpExp Ast.Minus left right) varMap =
  let
    leftAssembly = generateExp left varMap
    rightAssembly = generateExp right varMap
  in
    leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "popq %rcx\n" ++ "subq %rax, %rcx\nmovq %rcx, %rax"
generateExp (Ast.BinOpExp Ast.Multi left right) varMap =
  let
    leftAssembly = generateExp left varMap
    rightAssembly = generateExp right varMap
  in
    leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "popq %rcx\n" ++ "imulq %rcx, %rax\n"
generateExp (Ast.BinOpExp Ast.Div left right) varMap =
  let
    leftAssembly = generateExp left varMap
    rightAssembly = generateExp right varMap
  in
    leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "movq %rax, %rcx\npopq %rax\nmovq $0, %rdx\nidivq %rcx, %rax\n"
