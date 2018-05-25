module Ast
    ( Const(..),
      TypeDef(..),
      Id(..),
      Program(..),
      FuncDecl(..),
      FuncParam(..),
      FuncBody(..),
      Exp(..),
      BlockItem(..),
      Statement(..),
      Declaration(..),
      BinOp(..),
      Unop(..)
    ) where

data Const = Int Int | Char String | String String deriving (Show, Eq)
data TypeDef = IntType | CharType deriving (Show, Eq)
newtype Id = Id String deriving (Show, Eq)

data BinOp = Plus | Minus | Multi | Div | Ge | Le | Gt | Lt | Eq | NotEq | And | Or deriving (Show)
data Unop = Negate | Complement | Not deriving (Show)

newtype Program = Prog FuncDecl deriving (Show)
data FuncDecl = FuncDecl TypeDef Id [FuncParam]  FuncBody deriving (Show)
data FuncParam = FuncParam TypeDef Id deriving (Show)
newtype FuncBody = Body [BlockItem] deriving (Show)
data BlockItem = StatementItem Statement | DeclarationItem Declaration deriving (Show)
data Statement = Return
                 | ReturnVal Exp
                 | ExpStatement Exp
                 | IfStatement Exp Statement (Maybe Statement)
                 | CompoundStatement [BlockItem] deriving (Show)
data Declaration = Declaration Id (Maybe Exp) deriving (Show)
data Exp = ConstExp Const
           | UnopExp Unop Exp
           | BinOpExp BinOp Exp Exp
           | AssignExp Id Exp
           | VarExp Id deriving (Show)
