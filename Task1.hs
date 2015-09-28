module Main where

import Grammar
import System.IO
import Checking

main = do
    f <- readFile "task1.in"
    let proof = map read (lines f) :: [Expr]
    let annotated = check proof 0 proof
    checkAnnotationAndPrint (head annotated) (tail annotated) 0
    


check :: [Expr] -> Int -> [Expr] -> [AnnotatedExpr]
check [] _ _ = []
check (e:restProof) i proof 
    | isAxiom e > 0 = Axiom (isAxiom e) e : check restProof (i + 1) proof
    | otherwise = let (a, b) = findAB e 0 (head proof) (tail (take i proof)) (take i proof) in MP a b e : check restProof (i + 1) proof

findAB :: Expr -> Int -> Expr ->  [Expr] -> [Expr] ->  (Int, Int)

findAB e i (Impl a b) rest proof
    | length rest == 0 = (findA a 0 proof, i)
    | e == b = let k = (findA a 0 proof) in (if k >= 0 then (k, i) else (findAB e (i+1) (head rest) (tail rest) proof))
    | otherwise = findAB e (i + 1) (head rest) (tail rest) proof

findA :: Expr -> Int -> [Expr] -> Int
findA _ _ [] = -1
findA a i (e:proof)
    | a == e = i
    | otherwise = findA a (i + 1) proof
    

data AnnotatedExpr =
      Axiom Int Expr
    | MP Int Int Expr 
    | NotAnnotated Expr
    deriving (Show)

checkAnnotationAndPrint (Axiom i e) annotated j = do
    putStrLn $ "(" ++ (show (j + 1)) ++ ") " ++ (show e) ++ " axiom " ++ (show i)
    if (length annotated == 0) then putStrLn "Proof is correct"  else checkAnnotationAndPrint (head annotated) (tail annotated) (j + 1)


checkAnnotationAndPrint (MP a b e) annotated j = do
    let msg = if (a == -1) then "(" ++ (show (j + 1)) ++ ") " ++ (show e) ++ "\n" ++ "Proof is incorrect from statement " ++ (show (j + 1)) else "(" ++ (show (j + 1)) ++ ") " ++ (show e) ++ " MP " ++ (show (a + 1)) ++ ", " ++ (show (b + 1))
    putStrLn msg
    if (a /= -1 && (length annotated) == 0) then putStrLn "Proof is correct" else (if (a == -1 || (length annotated) == 0) then return () else checkAnnotationAndPrint (head annotated) (tail annotated) (j + 1))
