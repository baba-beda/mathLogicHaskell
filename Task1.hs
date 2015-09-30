module Main where

import Grammar
import System.IO
import Checking
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import qualified Data.Map as Map

main = do
    f <- readFile "task1.in"
    let proof = map read (lines f) :: [Expr]
    let res = checkProof proof
    printAnnotations (head res) (tail res) 0
    

checkProof proof  = runST $ do
    let n = (length proof)
    proofArray <- newListArray (0, n - 1) proof :: ST s (STArray s Int Expr)
    mapProof <- newSTRef Map.empty
    neededA <- newSTRef Map.empty
    resultsMP <- newSTRef Map.empty
    annotations <- forM [0..n - 1] $ \i -> do
        mPr <- readSTRef mapProof
        nA <- readSTRef neededA
        resMP <- readSTRef resultsMP
        curExp <- readArray proofArray i
        let a = isAxiom curExp
        let result = Wrong curExp
        if (a > 0) then do
            let result = Axiom a curExp
            return ()
        else do
            case Map.lookup curExp resMP of
                Nothing -> do
                    let result = Wrong curExp
                    return ()
                Just (f, s) -> do
                    let result = MP f s curExp
                    return ()

        case curExp of 
            (Impl a b) -> do
                    case Map.lookup a mPr of
                        Nothing -> writeSTRef neededA (Map.insert a i nA)
                        Just num -> writeSTRef resultsMP (Map.insert b (num, i) resMP)
            otherwise -> return () 
        case Map.lookup curExp nA of
            Nothing -> return ()
            Just index -> do
                Impl a b <- readArray proofArray index
                writeSTRef resultsMP (Map.insert b (i, index) resMP)
                writeSTRef neededA (Map.delete curExp nA)
        writeSTRef mapProof (Map.insert curExp i mPr)
        return result
    return annotations
        
data AnnotatedExpr =
      Axiom Int Expr
    | MP Int Int Expr
    | Wrong Expr


printAnnotations (Axiom i e) annotated j = do
    putStrLn $ "(" ++ (show (j + 1)) ++ ") " ++ (show e) ++ " axiom " ++ (show i)
    if (length annotated == 0) then putStrLn "Proof is correct"  else printAnnotations (head annotated) (tail annotated) (j + 1)

printAnnotations (MP a b e) annotated j = do
    putStrLn $ "(" ++ (show (j + 1)) ++ ") " ++ (show e) ++ " MP  " ++ (show (a + 1)) ++ ", " ++ (show (b + 1))
    if (length annotated == 0) then putStrLn "Proof is correct"  else printAnnotations (head annotated) (tail annotated) (j + 1)

printAnnotations (Wrong e) annotated j = do
    putStrLn $ "(" ++ (show (j + 1)) ++ ") " ++ (show e) ++ "\n" ++ "Proof is incorrect from statement " ++ (show (j + 1))
