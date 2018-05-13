module Parser
    (
      parse,
      parseStatements,
      parseExp,
    ) where

import Ast
import Lex

parse :: [Token] -> Program
parse tokens = Prog(parseFuncDecl tokens)

parseFuncDecl :: [Token] -> FuncDecl
parseFuncDecl (Lex.IntKeyword:Lex.Id name:Lex.OpenParen:rest) =
  let
    (funcType, funcName, rest1) = (Ast.IntType, Ast.Id name, rest)
  in
    let
      (params, rest2) = parseFuncParams rest1
    in
      let
        body = parseFuncBody rest2
      in
        Ast.FuncDecl funcType funcName params body


parseFuncParams :: [Token] -> ([FuncParam], [Token])
parseFuncParams (Lex.CloseParen:rest) = ([], rest)
parseFuncParams _ = error "not supported"

parseFuncBody :: [Token] -> FuncBody
parseFuncBody (Lex.OpenBrace:rest) =
  let
    (statements, rest1) = parseStatements rest
  in
    Ast.Body statements

parseStatements :: [Token] -> ([Statement], [Token])
parseStatements (Lex.Semicolon:rest) = parseStatements rest
parseStatements (Lex.ReturnKeyword:rest) =
  let
    (exp, rest1) = parseExp rest
  in
    let
      (otherStatements, rest2) = parseStatements rest1
    in
      (Ast.ReturnVal exp:otherStatements, rest2)
parseStatements (Lex.CloseBrace:rest) = ([], rest)
parseStatements [] = ([], [])

parseExp :: [Token] -> (Exp, [Token])
parseExp (Lex.Int i:rest) = (Ast.Exp(Ast.Int i), rest)
