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

-- variable map, stack index, end of the loop label, label for continue
type Context = (Map.Map String Int, Int, String, String)

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

  (varMap, index, endOfTheLoopLabel, labelForContinue) <- get
  put(varMap, index + 1, endOfTheLoopLabel, labelForContinue)
  let postLabel = "_label" ++ show index

  case elseBody of
    Nothing -> return (asmExp ++ "cmpq $0, %rax\nje " ++ postLabel ++ "\n" ++ asmStatement ++ "jmp " ++ postLabel ++ "\n" ++ postLabel ++ ":\n")
    Just a -> do
      (varMap, index, endOfTheLoopLabel, labelForContinue) <- get
      put(varMap, index + 1, endOfTheLoopLabel, labelForContinue)
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
generateStatement (Ast.WhileStatement exp statement) = do
  (originalVarMap, index, endOfTheLoopLabel, labelForContinue) <- get
  let label1 = "_label" ++ show index
  let label2 = "_label" ++ show (index + 1)
  put(originalVarMap, index + 2, label2, label1)

  asmExp <- generateExp exp
  statementAsm <- generateStatement statement

  let asm = label1 ++ ":\n" ++ asmExp ++ "\ncmpq $0, %rax\nje " ++ label2 ++ "\n" ++ statementAsm ++ "\n" ++ "jmp " ++ label1 ++ "\n" ++ label2 ++ ":\n"

  -- ループ終わりのラベルを元に戻す
  -- ループで新しく宣言されたローカル変数はここでスコープから外れる
  (_, index, _, _) <- get
  put(originalVarMap, index, endOfTheLoopLabel, labelForContinue)

  return asm
generateStatement (Ast.DoWhileStatement exp statement) = do
  (originalVarMap, index, endOfTheLoopLabel, labelForContinue) <- get
  let label1 = "_label" ++ show index
  put(originalVarMap, index + 1, endOfTheLoopLabel, labelForContinue)

  asmExp <- generateExp exp
  statementAsm <- generateStatement statement

  let asm = label1 ++ ":\n" ++ statementAsm ++ "\n" ++ asmExp ++ "\ncmpq $0, %rax\njne " ++ label1 ++ "\n"

  -- ループ終わりのラベルを元に戻す
  -- ループで新しく宣言されたローカル変数はここでスコープから外れる
  (_, index, _, _) <- get
  put(originalVarMap, index, endOfTheLoopLabel, labelForContinue)

  return asm
generateStatement (Ast.ExpOptionStatement exp) =
  case exp of
    Nothing -> return ""
    Just a -> generateExp a
generateStatement (Ast.ForStatement init condition postCond statement) = do
  (originalVarMap, index, endOfTheLoopLabel, labelForContinue) <- get
  let label1 = "_for_init" ++ show index
  let label2 = "_for_finish" ++ show (index + 1)
  let postLabel = "_for_post" ++ show (index + 2)
  -- for break
  put(originalVarMap, index + 3, label2, postLabel)

  initAsm <- generateStatement init
  condAsm <- generateCondStatement condition
  postAsm <- generateStatement postCond
  stmtAsm <- generateStatement statement

  let asm = initAsm ++ "\n" ++ label1 ++ ":\n" ++ condAsm ++ "\ncmpq $0, %rax\nje " ++ label2 ++ "\n" ++ stmtAsm ++ "\n" ++ postLabel ++ ":\n"++ postAsm ++ "\njmp " ++ label1 ++ "\n" ++ label2 ++ ":\n"

  -- ループ終わりのラベルを元に戻す
  -- ループで新しく宣言されたローカル変数はここでスコープから外れる
  (_, indexAfterLoop, _, _) <- get
  put(originalVarMap, indexAfterLoop, endOfTheLoopLabel, labelForContinue)

  return asm

generateStatement (Ast.ForWithDeclarationStatement init cond post statement) = do
  (originalVarMap, index, endOfTheLoopLabel, labelForContinue) <- get
  let label1 = "_init" ++ show index
  let label2 = "_finish" ++ show (index + 1)
  let postLabel = "_for_post" ++ show (index + 2)
  put(originalVarMap, index + 3, label2, postLabel)

  initAsm <- generateDeclaration init
  condAsm <- generateStatement cond
  postAsm <- generateStatement post
  stmtAsm <- generateStatement statement

  -- for()内のローカル変数のdeallocateをする
  let asm = initAsm ++ "\n" ++ label1 ++ ":\n" ++ condAsm ++ "\ncmpq $0, %rax\nje " ++ label2 ++ "\n" ++ stmtAsm ++ "\n" ++ postLabel ++ ":\n" ++ postAsm ++ "\njmp " ++ label1 ++ "\n" ++ label2 ++ ":\n" ++ "pop %rdx\n"

  -- ループ終わりのラベルを元に戻す
  -- ループで新しく宣言されたローカル変数はここでスコープから外れる
  (_, newIndex, _, _) <- get
  put(originalVarMap, newIndex, endOfTheLoopLabel, labelForContinue)

  return asm

generateStatement Ast.BreakStatement = do
  (varMap, index, endOfTheLoopLabel, labelForContinue) <- get
  return ("jmp " ++ endOfTheLoopLabel ++ "\n")

generateStatement Ast.ContinueStatement = do
  (varMap, index, endOfTheLoopLabel, labelForContinue) <- get
  return ("jmp " ++ labelForContinue ++ "\n")

generateCondStatement (Ast.ExpOptionStatement exp) =
  case exp of
    Nothing -> return "movq $1, %rax\n"
    Just a -> generateExp a

generateDeclaration :: Ast.Declaration -> State Context String
generateDeclaration (Ast.Declaration (Ast.Id id) exp) = do
  (varMap, index, endOfTheLoopLabel, labelForContinue) <- get
  let size = Map.size varMap
  let varMap1 = Map.insert id (-size*8 - 8) varMap
  put (varMap1, index, endOfTheLoopLabel, labelForContinue)

  case exp of
    Nothing -> return "movq $0, %rax\npushq %rax\n"
    Just a -> do
      asm <- generateExp a
      return ( asm ++ "pushq %rax\n")

generateExp :: Ast.Exp -> State Context String
generateExp (Ast.ConstExp(Ast.Int value)) = return ("movq $" ++ show value ++ ", %rax\n")
generateExp (Ast.VarExp (Ast.Id id)) = do
  (varMap, index, endOfTheLoopLabel, labelForContinue) <- get
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
  (varMap, index, label, _) <- get
  let offsetKey = Map.lookup id varMap
  case offsetKey of
    Nothing -> error "cannot find var"
    Just offset -> do
      asm <- generateExp exp
      return (asm ++ "movq %rax, " ++ show offset ++ "(%rbp)\n")
generateExp (Ast.ConditionalExp e1 e2 e3) = do
  -- create labels
  (varMap, index, endOfTheLoopLabel, labelForContinue) <- get
  let label1 = "_label" ++ show index
  let postLabel = "_label" ++ show (index + 1)
  put(varMap, index + 2, endOfTheLoopLabel, labelForContinue)

  e1Asm <- generateExp e1
  e2Asm <- generateExp e2
  e3Asm <- generateExp e3

  return (e1Asm ++ "cmpq $0, %rax\nje " ++ label1 ++ "\n" ++ e2Asm ++ "jmp " ++ postLabel ++ "\n" ++ label1 ++ ":\n" ++ e3Asm ++ postLabel ++ ":\n")
