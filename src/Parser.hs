module Parser
    (
      program,
      statement
    ) where

import Ast
import Lex

import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import Data.Char
import Control.Applicative
import Control.Monad (void)
import Text.Parsec (digit, letter, oneOf, satisfy, try, string)

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

semicolon = satisfy (== ';')
openbrace = satisfy (== '{')
closebrace = satisfy (== '}')
openparen = satisfy (== '(')
closeparen = satisfy (== ')')

-- number
num :: Parser Ast.Const
num = Ast.Int . read <$> many1 digit

-- typedef
typedef :: Parser Ast.TypeDef
typedef = do
  typeStr <- lexeme $ string "int" <|> string "char"
  case typeStr of
    "int" -> return Ast.IntType
    "char" -> return Ast.CharType

-- identifier
identifier :: Parser Ast.Id
identifier = Ast.Id <$> many1 letter

program :: Parser Ast.Program
program = do
  t <- lexeme typedef
  main <- lexeme identifier

  -- parameters
  openparen
  closeparen

  whitespace

  -- body
  lexeme openbrace
  stmt <- lexeme statement
  lexeme closebrace

  return (Ast.Prog(Ast.FuncDecl t main [] (Ast.Body [stmt])))

statement :: Parser Ast.Statement
statement = do
  -- return
  str <- lexeme $ string "return"
  -- expression
  exp <- expression
  semicolon
  return (Ast.ReturnVal exp)

-- <exp> ::= <term> { ("+" | "-") <term> }
expression :: Parser Ast.Exp
expression = try expBinOp <|> expConst
-- <term>
expConst :: Parser Ast.Exp
expConst = term

-- <term> { ("+" | "-") <term> }
expBinOp :: Parser Ast.Exp
expBinOp = do
  leftTerms <- term
  rightTerms <- try (many1 $ satisfy (== '+') *> term)
  let binop = foldr (Ast.BinOpExp Ast.Plus) (last rightTerms) (leftTerms:init rightTerms)
  return binop

-- <term> ::= <factor> { ("*" | "/") <factor> }
term :: Parser Ast.Exp
term = try termBinOp <|> termConst
-- <factor>
termConst :: Parser Ast.Exp
termConst = factor

-- <factor> { ("*" | "/") <factor> }
termBinOp :: Parser Ast.Exp
termBinOp = do
  leftFactor <- factor
  rightFactors <- try (many1 $ satisfy (== '*') *> factor)
  let binop = foldr (Ast.BinOpExp Ast.Multi) (last rightFactors) (leftFactor:init rightFactors)
  return binop

-- <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
factor :: Parser Ast.Exp
factor = try factorUnop <|> factorInt

factorUnop :: Parser Ast.Exp
factorUnop = do
  op <- oneOf "-!~"
  f <- factor
  case op of
    '-' -> return (Ast.UnopExp Ast.Negate f)
    '!' -> return (Ast.UnopExp Ast.Not f)
    '~' -> return (Ast.UnopExp Ast.Complement f)

factorInt :: Parser Ast.Exp
factorInt = Ast.ConstExp <$> lexeme num
