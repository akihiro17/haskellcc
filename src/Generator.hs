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


generateStatement :: Ast.Statement -> String
generateStatement (Ast.ReturnVal (Ast.Exp(Ast.Int value))) =
  "movq $" ++ show value ++ ", %rax\nret\n"

generateFunction :: Ast.FuncDecl -> String
generateFunction (Ast.FuncDecl funcType (Ast.Id funcName) funcParams (Ast.Body statements)) =
  let
    assembly = funcName ++ "\n" ++ funcName ++ ":\n"
    statementsAssembly = map generateStatement statements
  in
    foldl (++) assembly statementsAssembly
