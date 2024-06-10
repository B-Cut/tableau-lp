module Lexer (Operator(..), Token(..), tokenize, lookAhead, accept, operatorToChar) where

-- Esse módulo é responsável por converter a entrada de String para uma lista de Tokens que podem ser processados mais fácilmente

import Data.Char

data Operator = Impl | E | Ou
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokNeg
           | TokLParen
           | TokRParen
           | TokId String
           | TokSpace
           | TokEnd
     deriving (Show, Eq)



tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
    | c `elem` "&|>" = TokOp (operator c) : tokenize cs
    | c == '~'      = TokNeg : tokenize cs
    | c == '('      = TokLParen : tokenize cs
    | c == ')'      = TokRParen : tokenize cs
    | isAlpha c     = identifier c cs
    | isSpace c     = tokenize cs
    | otherwise     = error $ "Cannot tokenize " ++ [c]

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs) = c

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts

identifier c cs = let (str, cs') = span isAlphaNum cs in
                  TokId (c:str) : tokenize cs'

operator :: Char -> Operator
operator c | c == '>' = Impl
           | c == '&' = E
           | c == '|' = Ou

operatorToChar :: Operator -> String
operatorToChar op | op == Impl = ">"
                  | op == E    = "&"
                  | op == Ou   = "|"

