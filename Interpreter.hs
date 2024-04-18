{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE InstanceSigs #-}

module Interpreter where

import Stack

import qualified Data.Set as S
import Data.List
import Data.Maybe

data LambdaExpr = Term String | Abs String LambdaExpr | App LambdaExpr LambdaExpr deriving Eq

instance Show LambdaExpr where
    show :: LambdaExpr -> String
    show (Term x) = x
    show (Abs x e1) = "λ" ++ x ++ "." ++ show e1
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

-- determines where the current function application ends
traverseInput :: Integral a => String -> Stack Char -> a -> String -> String
traverseInput [] _ _ acc = reverse acc
traverseInput str@(h : t) stack initialSize acc =
    case h of
        '(' -> traverseInput t (stackPush '(' stack) initialSize ('(' : acc)
        ')' -> if stackSize stack == initialSize + 1 -- it's clear that I have found the entire expression
                    then reverse (')' : acc)
                    else traverseInput t (fst $ fromJust $ stackPop stack) initialSize (')' : acc)
        _ -> traverseInput t stack initialSize (h : acc)

nextToken :: String -> String
nextToken str =
    let
        expr = traverseInput str [] 0 []
        remainder = drop (length expr) str
    in
        if
            | null remainder -> []
            | head remainder == ' ' -> tail remainder
            | otherwise -> []

parseLambdaExpr :: String -> LambdaExpr
parseLambdaExpr [] = undefined
parseLambdaExpr str@(h : t) =
    case h of
        '(' -> let
                    e1 = traverseInput t [] 0 []
                in
                    if null (nextToken t)
                        then
                            let
                                e1' = takeWhile (/= ' ') e1
                                e2' = tail (dropWhile (/= ' ') e1)
                            in
                                App (parseLambdaExpr e1') (parseLambdaExpr e2')
                        else
                            let
                                e2 = init $ traverseInput (nextToken t) [] 0 []
                            in
                                App (parseLambdaExpr e1) (parseLambdaExpr e2)
        'λ' -> let
                    x = takeWhile (/= '.') t
                    e1 = tail (dropWhile (/= '.') t)
                in
                    Abs x (parseLambdaExpr e1)
        _ -> Term (takeWhile (`notElem` "λ().") str)

-- this function computes the leftmost outermost β-redex (i.e. the β-redex that is to be evaluated next)
nextRedex :: LambdaExpr -> Maybe LambdaExpr
nextRedex (Term _) = Nothing
nextRedex (Abs _ e1) = nextRedex e1
nextRedex (App e1 e2) =
    case e1 of
        Abs _ _ -> Just (App e1 e2)
        Term _ -> nextRedex e2
        App _ _ -> nextRedex e1

isBound :: String -> LambdaExpr -> Bool
isBound x (Abs y e) = x == y || isBound x e
isBound x (App e1 e2) = isBound x e1 && isBound x e2
isBound _ _ = False

isFree :: String -> LambdaExpr -> Bool
isFree x e = not (isBound x e)

allVariables' :: LambdaExpr -> [String]
allVariables' (Term x) = [x]
allVariables' (Abs x e1) = x : allVariables e1
allVariables' (App e1 e2) = allVariables e1 ++ allVariables e2

nub' :: Ord a => [a] -> [a]
nub' = S.toList . S.fromList

allVariables :: LambdaExpr -> [String]
allVariables = nub' . allVariables'

allBoundVariables :: LambdaExpr -> [String]
allBoundVariables expr = filter (`isBound` expr) (allVariables expr)

allFreeVariables :: LambdaExpr -> [String]
allFreeVariables expr = filter (`isFree` expr) (allVariables expr)

isRedex :: LambdaExpr -> Bool
isRedex (App (Abs _ _) _) = True
isRedex _ = False

αConversion :: LambdaExpr -> LambdaExpr
αConversion expr = if isRedex expr then renameVariables expr else error ("Cannot do α-conversion on " ++ show expr ++ " as it is not a β-redex.\n")
    where
        helper_remove :: LambdaExpr -> S.Set String -> LambdaExpr
        helper_remove expr vars = S.foldr (\element acc -> f element acc (S.size vars)) expr vars
            where
                f :: String -> LambdaExpr -> Int -> LambdaExpr
                f element acc count =
                    let
                        new_name = replicate count 'a'
                    in
                        case acc of
                            Term x -> if x == element then Term new_name else Term x
                            Abs x e1 -> Abs (if x == element then new_name else x) (f element e1 count)
                            App e1 e2 -> App (f element e1 count) (f element e2 count)

        renameVariables :: LambdaExpr -> LambdaExpr
        renameVariables expr@(App (Abs x e1) e2) =
            if not $ null intersect
                then App (Abs x (helper_remove e1 intersect)) e2
                else expr
            where
                intersect = S.fromList (allFreeVariables e2) `S.intersection` S.fromList (allBoundVariables e1)

        renameVariables _ = undefined

βReduction :: LambdaExpr -> LambdaExpr
βReduction expr = if isRedex expr then computeRedex expr else error ("Cannot do β-reduction on " ++ show expr ++ " as it is not a β-redex.\n")
    where
        computeRedex :: LambdaExpr -> LambdaExpr
        computeRedex (App (Abs x e1) e2) = replace e1 x e2
            where
                replace (Term y) x e2 = if y == x then e2 else Term y
                replace (Abs y e) x e2 = Abs y (replace e x e2)
                replace (App e1' e2') x e2 = App (replace e1' x e2) (replace e2' x e2)

        computeRedex e = e -- the β-reductions have finished

-- this function actually computes the normal form of a λ-expression
computeLambdaExpr :: LambdaExpr -> LambdaExpr
computeLambdaExpr expr =
    let
        redex = nextRedex expr

        compute' :: LambdaExpr -> LambdaExpr -> LambdaExpr
        compute' expr@(App (Abs x e1) e2) redex =
            if expr == redex
                then βReduction . αConversion $ redex
                else expr
        compute' (Term x) redex = Term x
        compute' (Abs x e1) redex = Abs x (compute' e1 redex)
        compute' (App e1 e2) redex =
            case e1 of
                Term x -> App e1 (compute' e2 redex)
                App _ _ -> App (compute' e1 redex) (compute' e2 redex)
    in
        if isNothing redex
            then expr -- normal form has been reached
            else computeLambdaExpr $ compute' expr (fromJust redex) -- do one evaluation step

evalLambdaExpr :: String -> LambdaExpr
evalLambdaExpr str = computeLambdaExpr $ parseLambdaExpr str