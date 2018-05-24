module Generator
  (
    generate,
    generateFunction,
    generateStatement,
    generateBlockItems,
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
generateFunction (Ast.FuncDecl funcType (Ast.Id funcName) funcParams (Ast.Body blockItems)) =
  let
    assembly = funcName ++ "\n" ++ funcName ++ ":\npushq %rbp\nmovq %rsp, %rbp\n"
    statementsAssembly = generateBlockItems blockItems Map.empty
  in
    assembly ++ statementsAssembly

generateBlockItems :: [Ast.BlockItem] -> Map.Map String Int -> String
generateBlockItems (Ast.StatementItem stmt:rest) varMap =
  let
    (asm, varMap1) = generateStatement stmt varMap
  in
    asm ++ generateBlockItems rest varMap1
generateBlockItems (Ast.DeclarationItem declaration:rest) varMap =
  let
    (asm, varMap1) = generateDeclaration declaration varMap
  in
    asm ++ generateBlockItems rest varMap1
generateBlockItems [] varMap = ""

-- <statement> ::= "return" <exp> ";" | <exp> ";" | "int" <id> [ = <exp>] ";"
generateStatement :: Ast.Statement -> Map.Map String Int -> (String, Map.Map String Int)
generateStatement (Ast.ReturnVal exp) varMap =
  (generateExp exp varMap ++ "movq %rbp, %rsp\npopq %rbp\nret\n", varMap)
generateStatement (Ast.ExpStatement exp) varMap =
  (generateExp exp varMap ++ "\n", varMap)

generateDeclaration :: Ast.Declaration -> Map.Map String Int -> (String, Map.Map String Int)
generateDeclaration (Ast.Declaration (Ast.Id id) exp) varMap =
  let
    size = Map.size varMap
    varMap1 = Map.insert id (-size*8 - 8) varMap
  in
    case exp of
      Nothing -> ("movq $0, %rax\npushq %rax\n", varMap1)
      Just a ->
        (generateExp a varMap ++ "\npushq %rax\n", varMap1)

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
    leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "popq %rcx\n" ++ "subq %rax, %rcx\nmovq %rcx, %rax\n"
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
generateExp (Ast.VarExp (Ast.Id id)) varMap =
  let
    offsetKey = Map.lookup id varMap
  in
    case offsetKey of
      Nothing -> error "cannot find var"
      Just offset -> "movq " ++ show offset ++ "(%rbp), %rax\n"
generateExp (Ast.AssignExp (Ast.Id id) exp) varMap =
  let
    offsetKey = Map.lookup id varMap
  in
    case offsetKey of
      Nothing -> error "cannot find var"
      Just offset -> generateExp exp varMap ++ "movq %rax, " ++ show offset ++ "(%rbp)\n"
