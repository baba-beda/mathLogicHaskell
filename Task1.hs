module Main where

import Grammar
import System.IO
import Checking

main = do
    f <- readFile "task1.in"
    let proof = lines f
    let a = read (head proof) :: Expr
    print a


