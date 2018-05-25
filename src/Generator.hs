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
import Control.Monad.State

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
    statementsAssembly = generateBlockItems blockItems Map.empty 0
  in
    assembly ++ statementsAssembly

generateBlockItems :: [Ast.BlockItem] -> Map.Map String Int -> Int -> String
generateBlockItems (Ast.StatementItem stmt:rest) varMap index =
  let
    (asm, varMap1, newIndex) = generateStatement stmt varMap index
  in
    asm ++ generateBlockItems rest varMap1 newIndex
generateBlockItems (Ast.DeclarationItem declaration:rest) varMap index =
  let
    (asm, varMap1) = generateDeclaration declaration varMap
  in
    asm ++ generateBlockItems rest varMap1 index
generateBlockItems [] varMap index = ""

-- <statement> ::= "return" <exp> ";" | <exp> ";" | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
generateStatement :: Ast.Statement -> Map.Map String Int -> Int -> (String, Map.Map String Int, Int)
generateStatement (Ast.ReturnVal exp) varMap index =
  (generateExp exp varMap ++ "movq %rbp, %rsp\npopq %rbp\nret\n", varMap, index)
generateStatement (Ast.ExpStatement exp) varMap index =
  (generateExp exp varMap ++ "\n", varMap, index)

--  cmpl $0, %eax ; check if EAX == 0
--  je _branch    ; if EAX == 0, go to _branch
--  movl $1, %eax
--  jump _post_if
-- _branch:
--  movl $2, %eax
-- _post_if
generateStatement (Ast.IfStatement cond body elseBody) varMap index =
  let
    asmExp = generateExp cond varMap
    (asmStatement, varMap1, newIndex1) = generateStatement body varMap index
    (postLabel, newIndex2) = freeLabel newIndex1
  in
    case elseBody of
      Nothing -> (asmExp ++ "cmpq $0, %rax\nje " ++ postLabel ++ "\n" ++ asmStatement ++ "jmp " ++ postLabel ++ "\n" ++ postLabel ++ ":\n", varMap1, newIndex2)
      Just a -> do
        let (label1, newIndex3) = freeLabel newIndex2
        let (asmElseBody, varMap2, newIndex4) = generateStatement a varMap1 newIndex3
        (asmExp ++ "cmpq $0, %rax\nje " ++ label1 ++ "\n" ++ asmStatement ++ "jmp " ++ postLabel ++ "\n" ++ label1 ++ ":\n" ++ asmElseBody ++ postLabel ++ ":\n", varMap2, newIndex4)

generateStatement (Ast.CompoundStatement blockItems) varMap index =
  let
    numberOfdeclaration = length [ x | x@(Ast.DeclarationItem _) <- blockItems ]
    asm = generateBlockItems blockItems varMap index
    deallocateAsm = unlines $ replicate numberOfdeclaration "popq %rdx"
  in
    (asm ++ deallocateAsm, varMap, index)

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
generateExp (Ast.VarExp (Ast.Id id)) varMap =
   let
     offsetKey = Map.lookup id varMap
   in
     case offsetKey of
       Nothing -> error "cannot find var"
       Just offset -> "movq " ++ show offset ++ "(%rbp), %rax\n"
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
generateExp (Ast.BinOpExp Ast.Or left right) varMap =
  let
    leftAssembly = generateExp left varMap
    rightAssembly = generateExp right varMap
  in
   leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "movq %rax, %rbx\npopq %rcx\norq %rcx, %rax\nmovq $0, %rax\nsetne %al\n"
generateExp (Ast.BinOpExp Ast.And left right) varMap =
  let
    leftAssembly = generateExp left varMap
    rightAssembly = generateExp right varMap
  in
   leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "popq %rcx\ncmpq $0, %rcx\nsetne %cl\ncmpq $0, %rax\nsetne %al\nandb %cl, %al\n"
generateExp (Ast.BinOpExp op left right) varMap =
  leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "movq %rax, %rbx\npopq %rcx\nmovq $0, %rax\ncmpq %rbx, %rcx\n" ++ setAsm ++ "\n"
  where
    leftAssembly = generateExp left varMap
    rightAssembly = generateExp right varMap
    setAsm = case op of
      Ast.Eq -> "sete %al"
      Ast.NotEq -> "setne %al"
      Ast.Gt -> "setg %al"
      Ast.Lt -> "setl %al"
      Ast.Ge -> "setge %al"
      Ast.Le -> "setle %al"
generateExp (Ast.AssignExp (Ast.Id id) exp) varMap =
  let
    offsetKey = Map.lookup id varMap
  in
    case offsetKey of
      Nothing -> error "cannot find var"
      Just offset -> generateExp exp varMap ++ "movq %rax, " ++ show offset ++ "(%rbp)\n"

freeLabel :: Int -> (String, Int)
freeLabel index = ("_label" ++ show index, index + 1)

uniqueLabel :: State Int String
uniqueLabel = do
  i <- get
  put(i+1)
  return ("_label" ++ show i)
