module Lex
    ( Token(..),
      lexer,
      tokens
    ) where

import Text.Regex.Posix

data Token = OpenBrace
             | CloseBrace
             | OpenParen
             | CloseParen
             | Semicolon
             | IntKeyword
             | CharKeyword
             | ReturnKeyword
             | Int Int
             | Id String
             | Minus
             | Complement
             | Neq

lexer :: Token -> String
lexer OpenBrace = "{"
lexer CloseBrace = "}"
lexer OpenParen = "("
lexer CloseParen = ")"
lexer Semicolon = ";"
lexer IntKeyword = "int"
lexer CharKeyword = "char"
lexer ReturnKeyword = "return"
lexer Minus = "-"
lexer Complement = "~"
lexer Neq = "!"
lexer (Id name) = name
lexer (Int value) = show value

tokens :: String -> [Token]
tokens "" = []
tokens input =
  let ch = head input
      rest = tail input
  in
    case ch of
     ' ' -> Lex.tokens rest
     '\n' -> Lex.tokens rest
     '\t' -> Lex.tokens rest
     '{' -> OpenBrace:Lex.tokens rest
     '}' -> CloseBrace:Lex.tokens rest
     '(' -> OpenParen:Lex.tokens rest
     ')' -> CloseParen:Lex.tokens rest
     ';' -> Semicolon:Lex.tokens rest
     '-' -> Minus:Lex.tokens rest
     '~' -> Complement:Lex.tokens rest
     '!' -> Neq:Lex.tokens rest
     '1' -> getInteger input
     '2' -> getInteger input
     '3' -> getInteger input
     '4' -> getInteger input
     '5' -> getInteger input
     '6' -> getInteger input
     '7' -> getInteger input
     '8' -> getInteger input
     '9' -> getInteger input
     _ -> getIdOrKeyword input

getIdOrKeyword :: String -> [Token]
getIdOrKeyword input =
  let
    id_regexp = "([A-Za-z][A-Za-z0-9_]*)"
    matched = input =~ id_regexp::String
  in
    case matched of
      "" -> Lex.tokens(drop 1 input)
      "int" -> IntKeyword:Lex.tokens(drop (length matched) input)
      "char" -> CharKeyword: Lex.tokens(drop (length matched) input)
      "return" -> ReturnKeyword:Lex.tokens(drop (length matched) input)
      _ -> Id matched:Lex.tokens(drop (length matched) input)

getInteger :: String -> [Token]
getInteger input =
  let
    id_regexp = "([1-9][0-9]*)"
    matched = input =~ id_regexp::String
  in
    Int(read matched :: Int):Lex.tokens(drop (length matched) input)
