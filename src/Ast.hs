module Ast
    ( Const(..),
      TypeDef(..),
      Id(..),
      Program(..),
      FuncDecl(..),
      FuncParam(..),
      FuncBody(..),
      Exp(..),
      Statement(..)
    ) where

data Const = Int Int | Char String | String String
data TypeDef = IntType | CharType deriving (Show, Eq)
newtype Id = Id String deriving (Show, Eq)

newtype Program = Prog FuncDecl
data FuncDecl = FuncDecl TypeDef Id [FuncParam]  FuncBody
data FuncParam = FuncParam TypeDef Id
newtype FuncBody = Body [Statement]
data Statement = Return
                 | ReturnVal Exp
newtype Exp = Exp Const