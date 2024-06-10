module Evaluator (tableau, initialFormula, getPaths, isValid) where

-- Esse módulo checa se houve contradições e a válidade das formulas

import Lexer
import Parser

-- Uma formula é ela mesma + sua validade
data Formula = Formula {
    definition :: Tree,
    valid :: Bool
} deriving Show
    
                 -- Tipo Esquerda Direita [formulas usadas] [formulas a usar]
data TableauTree = TableauFork TableauTree TableauTree [Formula] [Formula]
                 | TableauSingle TableauTree [Formula] [Formula]
                 -- Formula que gerou contradição, formulas usadas
                 | TableauContradiction Formula [Formula]
                 | TableauContinuation [Formula]
    deriving Show

-- Recebemos o nó atual, sua validade e uma variaveis de formulas
-- Retornamos se a formula atual é valida ou não

-- Precisamos de duas funções, uma para avaliar e outra para tirar formulas do varmap

initialFormula :: String -> Formula
initialFormula str = Formula (strToTree str) False

contradicts :: Formula -> [Formula] -> Bool
contradicts currentFormula (f : fl) =
    -- Redundante mas legível
    if definition f == definition currentFormula && valid f /= valid currentFormula
    then True
    else contradicts currentFormula fl

contradicts _ [] = False

tableau :: [Formula] -> [Formula] -> TableauTree
tableau usedFormulas unusedFormulas = 
    if null unusedFormulas
    then TableauContinuation usedFormulas
    else 
        let (currentFormula : unusedFormulas') = unusedFormulas
            usedFormulas' = currentFormula:usedFormulas
            parsedFormula = definition currentFormula
        in 
            if contradicts currentFormula usedFormulas
            then TableauContradiction currentFormula usedFormulas
            else
            case parsedFormula of
                -- Negação
                (NegNode inFormula) -> 
                    let 
                        newFormula = Formula inFormula (not $ valid currentFormula)
                        -- Na negação, a formula interior tem validade inversa a negação
                        newNode = tableau usedFormulas' (newFormula : unusedFormulas')
                    in TableauSingle newNode usedFormulas' unusedFormulas'
                -- Implicação
                -- Essa estrutura se repete várias vezes
                (OpNode Impl left right) -> 
                    if valid currentFormula
                    then
                        let formulaL = Formula left False
                            formulaR = Formula right True

                            nodeL = tableau usedFormulas' (formulaL:unusedFormulas')
                            nodeR = tableau usedFormulas' (formulaR:unusedFormulas')

                        in TableauFork nodeL nodeR usedFormulas' unusedFormulas'
                    else 
                        let formula = Formula left True
                            formula' = Formula right False

                            unusedFormulas'' = formula:formula':unusedFormulas'
                            node = tableau usedFormulas' unusedFormulas''
                        in TableauSingle node usedFormulas' unusedFormulas''

                -- E

                (OpNode E left right) -> 
                    if valid currentFormula
                    then 
                        let formula = Formula left True
                            formula' = Formula right True

                            unusedFormulas'' = formula:formula':unusedFormulas'
                            node = tableau usedFormulas' unusedFormulas''
                        in TableauSingle node usedFormulas' unusedFormulas''
                    else
                        let formulaL = Formula left False
                            formulaR = Formula right False

                            nodeL = tableau usedFormulas' (formulaL:unusedFormulas')
                            nodeR = tableau usedFormulas' (formulaR:unusedFormulas')

                        in TableauFork nodeL nodeR usedFormulas' unusedFormulas'
                    
                -- Ou

                (OpNode Ou left right) -> 
                    if valid currentFormula
                    then
                        let formulaL = Formula left True
                            formulaR = Formula right True

                            nodeL = tableau usedFormulas' (formulaL:unusedFormulas')
                            nodeR = tableau usedFormulas' (formulaR:unusedFormulas')

                        in TableauFork nodeL nodeR usedFormulas' unusedFormulas'
                    else 
                        let formula = Formula left False
                            formula' = Formula right False

                            unusedFormulas'' = formula:formula':unusedFormulas'
                            node = tableau usedFormulas' unusedFormulas''
                        in TableauSingle node usedFormulas' unusedFormulas''

                -- atomic
                (AtomicNode id) ->
                    let node = tableau usedFormulas' unusedFormulas'
                    in TableauSingle node usedFormulas' unusedFormulas'
                
getPaths :: TableauTree -> String
getPaths (TableauFork esq dir _ _) = 
    let pathEsq = getPaths esq
        pathDir = getPaths dir
    in 
        pathDir ++ "\n" ++ pathEsq 
getPaths (TableauSingle node _ _) = getPaths node

getPaths (TableauContinuation usedFormulas) =
    prettyPath usedFormulas ++ "Continuação"

getPaths (TableauContradiction formula usedFormulas) =
    prettyPath (formula:usedFormulas) ++ "Contradição"

prettyPath :: [Formula] -> String
prettyPath [] = ""
prettyPath (x:xs) = 
    prettyPath xs ++ "(" ++ treeToStr (definition x) ++ " : " ++ validity ++ ") " ++ " -> "
    where validity = if valid x
                     then "V"
                     else "F"

isValid :: TableauTree -> Bool
isValid tree = 
    case tree of
        TableauContinuation _ -> False
        TableauContradiction _ _ ->  True
        (TableauFork left right _ _) -> isValid left && isValid right
        (TableauSingle node _ _) -> isValid node