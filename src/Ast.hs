module Ast
    ( Const(..),
      TypeDef(..),
      Id(..),
      Program(..),
      FuncDecl(..),
      FuncParam(..),
      FuncBody(..),
      Exp(..),
      Statement(..),
      BinOp(..),
      Unop(..)
    ) where

data Const = Int Int | Char String | String String
data TypeDef = IntType | CharType deriving (Show, Eq)
newtype Id = Id String deriving (Show, Eq)

data BinOp = Plus | Minus | Multi | Div
data Unop = Negate | Complement | Not

newtype Program = Prog FuncDecl
data FuncDecl = FuncDecl TypeDef Id [FuncParam]  FuncBody
data FuncParam = FuncParam TypeDef Id
newtype FuncBody = Body [Statement]
data Statement = Return
                 | ReturnVal Exp
data Exp = ConstExp Const | UnopExp Unop Exp | BinOpExp BinOp Exp Exp
