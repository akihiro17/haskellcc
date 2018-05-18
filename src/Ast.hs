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

data Const = Int Int | Char String | String String deriving (Show, Eq)
data TypeDef = IntType | CharType deriving (Show, Eq)
newtype Id = Id String deriving (Show, Eq)

data BinOp = Plus | Minus | Multi | Div deriving (Show)
data Unop = Negate | Complement | Not deriving (Show)

newtype Program = Prog FuncDecl deriving (Show)
data FuncDecl = FuncDecl TypeDef Id [FuncParam]  FuncBody deriving (Show)
data FuncParam = FuncParam TypeDef Id deriving (Show)
newtype FuncBody = Body [Statement] deriving (Show)
data Statement = Return
                 | ReturnVal Exp deriving (Show)
data Exp = ConstExp Const | UnopExp Unop Exp | BinOpExp BinOp Exp Exp deriving (Show)
