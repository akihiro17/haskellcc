module Parser
    (
      program,
      blockItem,
      statement,
      expStatement
    ) where

import Ast

import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import Data.Char
import Control.Applicative
import Control.Monad (void)
import Text.Parsec (digit, letter, oneOf, satisfy, try, string, char, optionMaybe)

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
  stmts <- try (many $ lexeme blockItem)
  lexeme closebrace

  return (Ast.Prog(Ast.FuncDecl t main [] (Ast.Body stmts)))

-- <block-item> ::= <statement> | <declaration>
blockItem :: Parser Ast.BlockItem
blockItem = try declarationItem <|> try statementItem

statementItem :: Parser Ast.BlockItem
statementItem = Ast.StatementItem <$> statement

declarationItem :: Parser Ast.BlockItem
declarationItem = Ast.DeclarationItem <$> declaration

-- <statement> ::= "return" <exp> ";" | <exp> ";" | "if" "(" <exp> ")" <statement> [ "else" <statement> ] | "{" { <block-item> } "}
statement :: Parser Ast.Statement
statement = try returnStatement <|> try expStatement <|> try ifStatement <|> try compoundStatement

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

ifStatement :: Parser Ast.Statement
ifStatement = do
  lexeme $ string "if"

  lexeme openparen
  exp <- lexeme expression
  lexeme closeparen

  body <- lexeme statement

  ch <- optionMaybe (string "else")
  case ch of
    Nothing -> return (Ast.IfStatement exp body Nothing)
    Just a -> do
      whitespace
      elseBody <- lexeme statement
      return (Ast.IfStatement exp body (Just elseBody))

compoundStatement :: Parser Ast.Statement
compoundStatement = do
  lexeme openbrace
  blockItems <- try (many $ lexeme blockItem)
  lexeme closebrace
  return (Ast.CompoundStatement blockItems)

-- <declaration> ::= "int" <id> [ = <exp> ] ";"
declaration :: Parser Ast.Declaration
declaration = do
  -- int
  str <- lexeme $ string "int"
  -- id
  id <- lexeme identifier
  -- =
  whitespace
  ch <- oneOf ";="
  case ch of
    ';' -> return  (Ast.Declaration id Nothing)
    '=' -> do
      whitespace
      -- expression
      exp <- lexeme expression
      semicolon
      return (Ast.Declaration id (Just exp))

-- <exp> ::= <id> "=" <exp> | <conditional-exp>
-- <conditional-exp> ::= <logical-or-exp> | <logical-or-exp> "?" <exp> ":" <conditional-exp>
-- <logical-or-exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
-- <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
-- <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
-- <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
-- <additive-exp> ::= <term> { ("+" | "-") <term> }

expression :: Parser Ast.Exp
expression = try assignExpression <|> try conditionalExpression

conditionalExpression :: Parser Ast.Exp
conditionalExpression = try ternaryExpression <|> logicalOrExpression
  where
    ternaryExpression = do
      e1 <- lexeme logicalOrExpression
      lexeme $ char '?'
      e2 <- lexeme expression
      lexeme $ char ':'
      e3 <- lexeme conditionalExpression
      return (Ast.ConditionalExp e1 e2 e3)

logicalOrExpression :: Parser Ast.Exp
logicalOrExpression = logicalAndExpression `chainl1` logicalOrOp
  where
    logicalOrOp = do
      op <- string "||"
      whitespace
      case op of
        "||" -> return (Ast.BinOpExp Ast.Or)

logicalAndExpression :: Parser Ast.Exp
logicalAndExpression = equalityExpression `chainl1` logicalAndOp
  where
    logicalAndOp = do
      op <- string "&&"
      whitespace
      case op of
        "&&" -> return (Ast.BinOpExp Ast.And)

equalityExpression :: Parser Ast.Exp
equalityExpression = relationalExpression `chainl1` equalityOp
  where
    equalityOp = do
      op <- string "==" <|> string "!="
      whitespace
      case op of
        "==" -> return (Ast.BinOpExp Ast.Eq)
        "!=" -> return (Ast.BinOpExp Ast.NotEq)

relationalExpression :: Parser Ast.Exp
relationalExpression = additiveExpression `chainl1` relationalOp
  where
    relationalOp = do
      whitespace
      op1 <- char '<' <|> char '>'
      op2 <- optionMaybe (char '=')
      whitespace
      case op2 of
        Nothing ->
          if op1 == '>' then return (Ast.BinOpExp Ast.Gt) else return (Ast.BinOpExp Ast.Lt)
        Just a ->
          if op1 == '>' then return (Ast.BinOpExp Ast.Ge) else return (Ast.BinOpExp Ast.Le)

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

-- <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int> | <id>
factor :: Parser Ast.Exp
factor = try factorExpression <|> try factorUnop <|> try factorInt <|> factorVar

factorExpression :: Parser Ast.Exp
factorExpression = lexeme openparen >> lexeme expression >>= \exp -> lexeme closeparen >> return exp

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
