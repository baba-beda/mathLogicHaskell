module Main where

import Grammar
import System.IO
import Checking
import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Array.Unboxed
import Data.Array.ST
import Data.STRef
import qualified Data.Map as Map
import System.Exit (exitSuccess)

main = do
    f <- readFile "task1.in"
    let proof = map read (lines f) :: [Expr]
    checkProof proof
    

checkProof proof  = runST $ do
    let n = (length proof)
    proofArray <- newListArray (0, n - 1) proof :: ST s (STArray s Int Expr)
    mapProof <- newSTRef Map.empty
    neededA <- newSTRef Map.empty
    resultsMP <- newSTRef Map.empty
    annotations <- newSTRef []
    annots <- forM_ [0..n - 1] $ \i -> do
        mPr <- readSTRef mapProof
        nA <- readSTRef neededA
        resMP <- readSTRef resultsMP
        curExp <- readArray proofArray i
        let a = isAxiom curExp
        an <- readSTRef annotations

        if (a > 0) then do
            let msg = "(" ++ (show (i + 1)) ++ ") " ++ (show curExp) ++ " axiom " ++ (show a)
            return msg
        else do
            case Map.lookup curExp resMP of
                Nothing -> do
                    let msg = "(" ++ (show (i + 1)) ++ ") " ++ (show curExp) ++ "\n The proof is incorrect from statement " ++ (show (i + 1))
                    return msg
                Just (f, s) -> do
                    let msg = "(" ++ (show (i + 1)) ++ ") " ++ (show curExp) ++ " MP " ++ (show (f + 1)) ++ ", " ++ (show (s + 1))
                    return msg

        case curExp of 
            (Impl a b) -> do
                    case Map.lookup a mPr of
                        Nothing -> writeSTRef neededA (Map.insert a i nA)
                        Just num -> writeSTRef resultsMP (Map.insert b (num, i) resMP)
        case Map.lookup curExp nA of
            Nothing -> return ()
            Just index -> do
                Impl a b <- readArray proofArray index
                writeSTRef resultsMP (Map.insert b (i, index) resMP)
                writeSTRef neededA (Map.delete curExp nA)
        writeSTRef mapProof (Map.insert curExp i mPr)
    return annots
        

