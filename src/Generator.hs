module Generator
  (
    generate,
    generateStatement,
    generateBlockItems
  ) where

import Data.List
import Ast
import qualified Data.Map as Map
import Control.Monad.State

type Context = (Map.Map String Int, Int)

generate :: Ast.Program -> State Context String
generate (Ast.Prog decl) = do
  let assembly = ".globl "
  s <- generateFunction decl
  return (assembly ++ s)

generateFunction :: Ast.FuncDecl -> State Context String
generateFunction (Ast.FuncDecl funcType (Ast.Id funcName) funcParams (Ast.Body blockItems)) = do
  blockItemsAssembly <- generateBlockItems blockItems
  return (funcName ++ "\n" ++ funcName ++ ":\npushq %rbp\nmovq %rsp, %rbp\n" ++ blockItemsAssembly)

generateBlockItems :: [Ast.BlockItem] -> State Context String
generateBlockItems (Ast.StatementItem stmt:rest) = do
  asm <- generateStatement stmt
  restAsm <- generateBlockItems rest
  return (asm ++ restAsm)
generateBlockItems (Ast.DeclarationItem declaration:rest) = do
  asm <- generateDeclaration declaration
  restAsm <- generateBlockItems rest
  return (asm ++ restAsm)
generateBlockItems [] = return ""

generateStatement :: Ast.Statement -> State Context String
generateStatement (Ast.ReturnVal exp) = do
  asm <- generateExp exp
  return (asm ++ "movq %rbp, %rsp\npopq %rbp\nret\n")
generateStatement (Ast.IfStatement cond body elseBody) = do
  asmExp <- generateExp cond
  asmStatement <- generateStatement body

  (varMap, index) <- get
  put(varMap, index + 1)
  let postLabel = "_label" ++ show index

  case elseBody of
    Nothing -> return (asmExp ++ "cmpq $0, %rax\nje " ++ postLabel ++ "\n" ++ asmStatement ++ "jmp " ++ postLabel ++ "\n" ++ postLabel ++ ":\n")
    Just a -> do
      (varMap, index) <- get
      put(varMap, index + 1)
      let label1 = "_label" ++ show index

      asmElseBody <- generateStatement a
      return (asmExp ++ "cmpq $0, %rax\nje " ++ label1 ++ "\n" ++ asmStatement ++ "jmp " ++ postLabel ++ "\n" ++ label1 ++ ":\n" ++ asmElseBody ++ postLabel ++ ":\n")
generateStatement (Ast.ExpStatement exp) = do
  asm <- generateExp exp
  return ( asm ++ "\n")
generateStatement (Ast.CompoundStatement blockItems) = do
  originalState <- get

  let numberOfdeclaration = length [ x | x@(Ast.DeclarationItem _) <- blockItems ]
  asm <- generateBlockItems blockItems
  let deallocateAsm = unlines $ replicate numberOfdeclaration "popq %rdx"

  put originalState
  return (asm ++ deallocateAsm)

generateDeclaration :: Ast.Declaration -> State Context String
generateDeclaration (Ast.Declaration (Ast.Id id) exp) = do
  (varMap, index) <- get
  let size = Map.size varMap
  let varMap1 = Map.insert id (-size*8 - 8) varMap
  put (varMap1, index)

  case exp of
    Nothing -> return "movq $0, %rax\npushq %rax\n"
    Just a -> do
      asm <- generateExp a
      return ( asm ++ "\npushq %rax\n")

generateExp :: Ast.Exp -> State Context String
generateExp (Ast.ConstExp(Ast.Int value)) = return ("movq $" ++ show value ++ ", %rax\n")
generateExp (Ast.VarExp (Ast.Id id)) = do
  (varMap, index) <- get
  let offsetKey = Map.lookup id varMap
  case offsetKey of
    Nothing -> error "cannot find var"
    Just offset -> return ("movq " ++ show offset ++ "(%rbp), %rax\n")
generateExp (Ast.UnopExp Ast.Negate exp) = do
  expAssembly <- generateExp exp
  return (expAssembly ++ "neg %rax\n")
generateExp (Ast.UnopExp Ast.Complement exp) = do
  expAssembly <- generateExp exp
  return (expAssembly ++ "not %rax\n")
generateExp (Ast.UnopExp Ast.Not exp) = do
  expAssembly <- generateExp exp
  return (expAssembly ++ "cmpq $0, %rax\nmovq $0, %rax\nsete %al\n")
generateExp (Ast.BinOpExp Ast.Plus left right) = do
  leftAssembly <- generateExp left
  rightAssembly <- generateExp right
  return (leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "popq %rcx\n" ++ "addq %rcx, %rax\n")
generateExp (Ast.BinOpExp Ast.Minus left right) = do
  leftAssembly <- generateExp left
  rightAssembly <- generateExp right
  return (leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "popq %rcx\n" ++ "subq %rax, %rcx\nmovq %rcx, %rax\n")
generateExp (Ast.BinOpExp Ast.Multi left right) = do
  leftAssembly <- generateExp left
  rightAssembly <- generateExp right
  return (leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "popq %rcx\n" ++ "imulq %rcx, %rax\n")
generateExp (Ast.BinOpExp Ast.Div left right) = do
  leftAssembly <- generateExp left
  rightAssembly <- generateExp right
  return (leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "movq %rax, %rcx\npopq %rax\nmovq $0, %rdx\nidivq %rcx, %rax\n")
generateExp (Ast.BinOpExp Ast.Or left right) = do
  leftAssembly <- generateExp left
  rightAssembly <- generateExp right
  return (leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "movq %rax, %rbx\npopq %rcx\norq %rcx, %rax\nmovq $0, %rax\nsetne %al\n")
generateExp (Ast.BinOpExp Ast.And left right) = do
  leftAssembly <- generateExp left
  rightAssembly <- generateExp right

  return (leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "popq %rcx\ncmpq $0, %rcx\nsetne %cl\ncmpq $0, %rax\nsetne %al\nandb %cl, %al\n")
generateExp (Ast.BinOpExp op left right) = do
  leftAssembly <- generateExp left
  rightAssembly <- generateExp right
  let setAsm = case op of
        Ast.Eq -> "sete %al"
        Ast.NotEq -> "setne %al"
        Ast.Gt -> "setg %al"
        Ast.Lt -> "setl %al"
        Ast.Ge -> "setge %al"
        Ast.Le -> "setle %al"
  return (leftAssembly ++ "pushq %rax\n" ++ rightAssembly ++ "movq %rax, %rbx\npopq %rcx\nmovq $0, %rax\ncmpq %rbx, %rcx\n" ++ setAsm ++ "\n")
generateExp (Ast.AssignExp (Ast.Id id) exp) = do
  (varMap, index) <- get
  let offsetKey = Map.lookup id varMap
  case offsetKey of
    Nothing -> error "cannot find var"
    Just offset -> do
      asm <- generateExp exp
      return (asm ++ "movq %rax, " ++ show offset ++ "(%rbp)\n")
-- runState (generate a) (Data.Map.empty, 0)
-- let (Right a) = parse Parser.program "" "int main() {return 2;}"
