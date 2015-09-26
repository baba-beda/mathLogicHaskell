module Main where

import Grammar
import System.IO

main = do
    print "5"


isAxiom :: Expr -> Int
isAxiom expr = (isAxiom1 expr) + (isAxiom2 expr) + (isAxiom3 expr) + (isAxiom4 expr) + (isAxiom5 expr) + (isAxiom6 expr) + (isAxiom7 expr) + (isAxiom8 expr) + (isAxiom9 expr) + (isAxiom10 expr)

isAxiom1 :: Expr -> Int
isAxiom1 (Impl a (Impl b c))
    | a == c = 1
    | otherwise = 0
isAxiom1 _ = 0

isAxiom2 :: Expr -> Int
isAxiom2 (Impl (Impl a b) (Impl (Impl c (Impl d e)) (Impl f g)))
    | a == c && c == f && b == d && e == g = 2
    | otherwise = 0
isAxiom2 _ = 0

isAxiom3 :: Expr -> Int
isAxiom3 (Impl a (Impl b (And c d)))
    | a == c && b == d = 3
    | otherwise = 0
isAxiom3 _ = 0

isAxiom4 :: Expr -> Int
isAxiom4 (Impl (And a b) c)
    | a == c = 4
    | otherwise = 0
isAxiom4 _ = 0

isAxiom5 :: Expr -> Int
isAxiom5 (Impl (And a b) c)
    | b == c && a /= b = 5
    | otherwise = 0
isAxiom5 _ = 0

isAxiom6 :: Expr -> Int
isAxiom6 (Impl a (Or b c))
    | a == b = 6
    | otherwise = 0
isAxiom6 _ = 0

isAxiom7 :: Expr -> Int
isAxiom7 (Impl a (Or b c))
    | a == c && a /= b = 7
    | otherwise = 0
isAxiom7 _ = 0

isAxiom8 :: Expr -> Int
isAxiom8 (Impl (Impl a b) (Impl (Impl c d) (Impl (Or e f) g)))
    | a == e && b == d && b == g && c == f = 8
    | otherwise = 0
isAxiom8 _ = 0

isAxiom9 ::  Expr -> Int
isAxiom9 (Impl (Impl a b) (Impl (Impl c (Not d)) (Not e)))
    | a == c && a == e && b == d = 9
    | otherwise = 0
isAxiom9 _ = 0

isAxiom10 :: Expr -> Int
isAxiom10 (Impl (Not (Not a)) b) 
    | a == b = 10
    | otherwise = 0
isAxiom10 _ = 0
