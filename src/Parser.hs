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
  exp <- lexeme expression
  semicolon
  return (Ast.ReturnVal exp)

-- <exp> ::= <term> { ("+" | "-") <term> }
expression :: Parser Ast.Exp
expression = try (expPlus <|> expMinus) <|> expConst
-- <term>
expConst :: Parser Ast.Exp
expConst = term

-- <term> { ("+" | "-") <term> }
expPlus :: Parser Ast.Exp
expPlus = do
  whitespace
  leftTerms <- term
  whitespace
  rightTerms <- try (many1 $ satisfy (=='+') *> whitespace *> term)
  let binop = foldr (Ast.BinOpExp Ast.Plus) (last rightTerms) (leftTerms:init rightTerms)
  whitespace
  return binop
expMinus :: Parser Ast.Exp
expMinus = do
  whitespace
  leftTerms <- term
  whitespace
  rightTerms <- try (many1 $ satisfy (=='-') *> whitespace *> term)
  let binop = foldr (Ast.BinOpExp Ast.Minus) (last rightTerms) (leftTerms:init rightTerms)
  whitespace
  return binop

-- <term> ::= <factor> { ("*" | "/") <factor> }
term :: Parser Ast.Exp
term = try termMulti <|> try termDiv <|> termConst
-- <factor>
termConst :: Parser Ast.Exp
termConst = lexeme factor

-- <factor> { ("*" | "/") <factor> }
termMulti :: Parser Ast.Exp
termMulti = do
  whitespace
  leftFactor <- lexeme factor
  rightFactors <- try (many1 $ satisfy (== '*') *> whitespace *> factor)
  let binop = foldr (Ast.BinOpExp Ast.Multi) (last rightFactors) (leftFactor:init rightFactors)
  whitespace
  return binop
termDiv :: Parser Ast.Exp
termDiv = do
  whitespace
  leftFactor <- lexeme factor
  rightFactors <- try (many1 $ satisfy (== '/') *> whitespace *> factor)
  let binop = foldr (Ast.BinOpExp Ast.Div) (last rightFactors) (leftFactor:init rightFactors)
  whitespace
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
