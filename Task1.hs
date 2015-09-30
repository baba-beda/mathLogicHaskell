module Main where

import Grammar
import Checking
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import qualified Data.Map as Map

main :: IO ()
main = do
    f <- readFile "task1.in"
    let proof = map read (lines f) :: [Expr]
    let res = checkProof proof
    printAnnotations (head res) (tail res) 0
    
checkProof :: [Expr] -> [AnnotatedExpr]
checkProof proof  = runST $ do
    let n = (length proof)
    proofArray <- newListArray (0, n - 1) proof :: ST s (STArray s Int Expr)
    mapProof <- newSTRef Map.empty
    neededA <- newSTRef Map.empty
    resultsMP <- newSTRef Map.empty
    auxForMsg <- newSTRef Null
    annotations <- forM [0..n - 1] $ \i -> do
        mPr <- readSTRef mapProof
        nA <- readSTRef neededA
        resMP <- readSTRef resultsMP
        curExp <- readArray proofArray i
        let a = isAxiom curExp
        if (a > 0) then do
            writeSTRef auxForMsg (Axiom a curExp)
        else do
            case Map.lookup curExp resMP of
                Nothing -> do
                    writeSTRef auxForMsg (Wrong curExp)
                Just (f, s) -> do
                    writeSTRef auxForMsg (MP f s curExp)
        case curExp of 
            (Impl alpha beta) -> do
                    case Map.lookup alpha mPr of
                        Nothing -> writeSTRef neededA (Map.insert alpha i nA)
                        Just num -> writeSTRef resultsMP (Map.insert beta (num, i) resMP)
            _ -> return () 
        case Map.lookup curExp nA of
            Nothing -> return ()
            Just ind -> do
                Impl _ beta <- readArray proofArray ind
                writeSTRef resultsMP (Map.insert beta (i, ind) resMP)
                writeSTRef neededA (Map.delete curExp nA)
        writeSTRef mapProof (Map.insert curExp i mPr)
        readSTRef auxForMsg
    return annotations
        
data AnnotatedExpr =
      Axiom Int Expr
    | MP Int Int Expr
    | Wrong Expr
    | Null
    deriving (Show)

printAnnotations :: AnnotatedExpr -> [AnnotatedExpr] -> Int -> IO ()
printAnnotations (Axiom i e) annotated j = do
    putStrLn $ "(" ++ (show (j + 1)) ++ ") " ++ (show e) ++ " axiom " ++ (show i)
    if (length annotated == 0) then putStrLn "Proof is correct"  else printAnnotations (head annotated) (tail annotated) (j + 1)

printAnnotations (MP a b e) annotated j = do
    putStrLn $ "(" ++ (show (j + 1)) ++ ") " ++ (show e) ++ " MP  " ++ (show (a + 1)) ++ ", " ++ (show (b + 1))
    if (length annotated == 0) then putStrLn "Proof is correct"  else printAnnotations (head annotated) (tail annotated) (j + 1)

printAnnotations (Wrong e) _ j = do
    putStrLn $ "(" ++ (show (j + 1)) ++ ") " ++ (show e) ++ "\n" ++ "Proof is incorrect from statement " ++ (show (j + 1))

printAnnotations Null _ _ = return ()
