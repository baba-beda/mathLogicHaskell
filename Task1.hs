module Main where

import Grammar
import System.IO
import Checking

main = do
    f <- readFile "task1.in"
    let proof = lines f
    putStrLn $ show (head proof)
    


check :: [Expr] -> Int -> [Expr] -> [AnnotatedExpr]
check (e:restProof) i proof 
    | isAxiom e > 0 = Axiom (isAxiom e) e : check restProof (i + 1) proof
    | otherwise = let (a, b) = findAB e 0 (head proof) (tail (take i proof)) (take i proof) in MP a b e : check restProof (i + 1) proof
check [] _ _ = []

findAB :: Expr -> Int -> Expr ->  [Expr] -> [Expr] ->  (Int, Int)
findAB e i (Impl a b) rest proof
    | e == b = let k = (findA a 0 proof) in (if k > 0 then (i, k) else (findAB e (i+1) (head rest) (tail rest) proof))
    | otherwise = findAB e (i + 1) (head rest) (tail rest) proof
findAB _ _ _ [] _ = (-1, -1)

findA :: Expr -> Int -> [Expr] -> Int
findA a i (e:proof)
    | a == e = i
    | otherwise = findA a (i + 1) proof
findA _ _ [] = -1
    

data AnnotatedExpr =
      Axiom Int Expr
    | MP Int Int Expr 
    | NotAnnotated Expr
    deriving (Show)
