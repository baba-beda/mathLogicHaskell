module Main where

import Grammar
import System.IO
import Checking
import Data.Array.ST
import Data.STRef
import qualified Data.Map as Map
import System.Exit (exitSuccess)

main = do
    f <- readFile "task1.in"
    let proof = map read (lines f) :: [Expr]
    checkProof proof

checkProof proof = runSTArray $ do
    let n = (length proof)
    proofArray <- newArray (0, n - 1) proof :: ST s (STArray s Int Expr)
    mapProof <- newSTRef Map.empty
    neededA <- newSTRef Map.empty
    resultsMP <- newSTRef Map.empty 
    forM_ [0..n - 1] $ \i -> do
        mPr <- readSTRef mapProof
        nA <- readSTRef neededA
        resMP <- readSTRef resultsMP
        curExp <- readArray proofArray i
        putStr $ "(" ++ (show (i + 1)) ++ ") " ++ (show curExp) ++ " "
        let a = isAxiom curExp

        if (a > 0) then do
            putStr $ "axiom " ++ (show a)
        else do
            if (Map.member curExp resMP) then do
                (f, s) <- lookup curExp resMP
                putStr $ "MP " ++ (show (f + 1)) ++ ", " ++ (show (s + 1))
            else do
                putStr $ "The proof is incorrect from statement " ++ (show (i + 1))
                exitSuccess

        case curExp of (Impl a b) -> do
            if (Map.member a mPr) then do
                num <- Map.lookup a mPr
                writeSTRef resultsMP (Map.insert b (num, i))
            else writeSTRef neededA (Map.insert nA a i)
        if (Map.member curExp neededA) then do
            index <- Map.lookup curExp neededA
            Impl a b <- readArray proofArray index
            writeSTRef resultsMP (Map.insert b (i, index))
            writeSTRef neededA (Map.delete nA curExp)
        else
            return ()
        writeSTRef mapProof (Map.insert curExp i)


