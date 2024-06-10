module Parser(Tree(..), parse, treeToStr, strToTree) where

import Lexer

data Tree = OpNode Operator Tree Tree
          | NegNode Tree
          | AtomicNode String
    deriving (Show, Eq)

parse :: [Token] -> Tree
parse toks = let (tree, toks') = unary toks
             in
                if null toks'
                then tree
                else error $ "Tokens de sobra: " ++ show toks'

unary :: [Token] -> (Tree, [Token])
unary tokens =
    case lookAhead tokens of
        TokNeg ->
            let (leaf, tokens') = operand (accept tokens)
            in (NegNode leaf, tokens')
        _ -> binary tokens

binary :: [Token] -> (Tree, [Token])
binary tokens = 
    let (operandTree, tokens') = operand tokens in
    case lookAhead tokens' of
        (TokOp op) ->
            let (formTree, tokens'') = operand (accept tokens')
            in (OpNode op operandTree formTree, tokens'')
        _ -> (operandTree, tokens')
        

operand :: [Token] -> (Tree, [Token])
operand tokens =
    case lookAhead tokens of
    -- base of recursion
        (TokId str)-> (AtomicNode str, accept tokens)

        TokLParen ->
            let (formTree, tokens') = unary (accept tokens)
            in
                if lookAhead tokens' /= TokRParen
                then error "Sem parentese direito"
                else (formTree, accept tokens')
        
        TokEnd -> error "Chegou ao fim"

        (TokOp op) -> error "Encontrou operador"

        TokRParen -> error "Parenteses direito"

        _ -> error "Não foi possível realizar o criar a árvore"

treeToStr :: Tree -> String
treeToStr tree =
    case tree of
        (AtomicNode str) -> str

        (OpNode operator operand operand') ->
            "(" ++ treeToStr operand ++ operatorToChar operator ++ treeToStr operand' ++ ")"
        (NegNode operand) ->
            "(~" ++ treeToStr operand ++ ")" 

strToTree :: String -> Tree
strToTree = parse . tokenize 