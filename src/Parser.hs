module Parser
    (
      program,
      statement,
      expStatement
    ) where

import Ast

import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import Data.Char
import Control.Applicative
import Control.Monad (void)
import Text.Parsec (digit, letter, oneOf, satisfy, try, string, char)

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
  stmts <- try (many $ lexeme statement)
  lexeme closebrace

  return (Ast.Prog(Ast.FuncDecl t main [] (Ast.Body stmts)))

statement :: Parser Ast.Statement
statement = try declareStatement <|> try returnStatement <|> expStatement

returnStatement :: Parser Ast.Statement
returnStatement = do
  -- return
  str <- lexeme $ string "return"
  -- expression
  exp <- lexeme expression
  semicolon
  return (Ast.ReturnVal exp)

expStatement :: Parser Ast.Statement
expStatement = do
  exp <- expression
  semicolon
  return (Ast.ExpStatement exp)

declareStatement :: Parser Ast.Statement
declareStatement = do
  -- int
  str <- lexeme $ string "int"
  -- id
  id <- lexeme identifier
  -- =
  whitespace
  ch <- oneOf ";="
  case ch of
    ';' -> return (Ast.DeclareStatement id Nothing)
    '=' -> do
      whitespace
      -- expression
      exp <- lexeme expression
      semicolon
      return (Ast.DeclareStatement id (Just exp))

expression :: Parser Ast.Exp
expression = try assignExpression <|> try additiveExpression

additiveExpression :: Parser Ast.Exp
additiveExpression = term `chainl1` addOp
  where
    addOp = do
      op <- char '+' <|> char '-'
      whitespace
      case op of
        '+' -> return (Ast.BinOpExp Ast.Plus)
        '-' -> return (Ast.BinOpExp Ast.Minus)

assignExpression :: Parser Ast.Exp
assignExpression = do
  -- id
  id <- lexeme identifier
  whitespace
  char '='
  whitespace
  -- expression
  exp <- lexeme expression
  return (Ast.AssignExp id exp)

-- <term> ::= <factor> { ("*" | "/") <factor> }
term :: Parser Ast.Exp
term = factor `chainl1` mulOp
  where
    mulOp = do
      op <- char '*' <|> char '/'
      whitespace
      case op of
        '*' -> return (Ast.BinOpExp Ast.Multi)
        '/' -> return (Ast.BinOpExp Ast.Div)

-- <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
factor :: Parser Ast.Exp
factor = try factorUnop <|> try factorInt <|> factorVar

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

factorVar :: Parser Ast.Exp
factorVar = Ast.VarExp <$> lexeme identifier
