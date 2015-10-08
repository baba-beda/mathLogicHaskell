module Annotator where

import Grammar
import Checking
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import qualified Data.Map as Map
import qualified Data.Set as Set


annotateProof :: [Expr] -> [Expr] -> [AnnotatedExpr]
annotateProof assumptions proof  = runST $ do
    let n = (length proof)
    proofArray <- newListArray (0, n - 1) proof :: ST s (STArray s Int Expr)
    mapProof <- newSTRef Map.empty
    neededA <- newSTRef Map.empty
    resultsMP <- newSTRef Map.empty
    auxForMsg <- newSTRef Null
    assumptionsST <- newSTRef (Set.fromList assumptions)
    annotations <- forM [0..n - 1] $ \i -> do
        mPr <- readSTRef mapProof
        nA <- readSTRef neededA
        resMP <- readSTRef resultsMP
        curExp <- readArray proofArray i
        asmpt <- readSTRef assumptionsST
        let a = isAxiom curExp
        if a > 0 then
            writeSTRef auxForMsg (Axiom a curExp)
        else
            if Set.member curExp asmpt then
                writeSTRef auxForMsg (Assumption curExp)
            else
                case Map.lookup curExp resMP of
                    Nothing -> writeSTRef auxForMsg (Wrong curExp)
                    Just (f, s) -> writeSTRef auxForMsg (MP f s curExp)
        case curExp of 
            (Impl alpha beta) ->
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
    | Assumption Expr
    | Wrong Expr
    | Null
    deriving (Show)

printAnnotations :: AnnotatedExpr -> [AnnotatedExpr] -> Int -> IO ()
printAnnotations (Axiom i e) annotated j = do
    putStrLn $ "(" ++ show (j + 1) ++ ") " ++ show e ++ " axiom " ++ show i
    if null annotated then putStrLn "Proof is correct"  else printAnnotations (head annotated) (tail annotated) (j + 1)

printAnnotations (MP a b e) annotated j = do
    putStrLn $ "(" ++ show (j + 1) ++ ") " ++ show e ++ " MP  " ++ show (a + 1) ++ ", " ++ show (b + 1)
    if null annotated then putStrLn "Proof is correct"  else printAnnotations (head annotated) (tail annotated) (j + 1)

printAnnotations (Wrong e) _ j = putStrLn $ "(" ++ show (j + 1) ++ ") " ++ show e ++ "\n" ++ "Proof is incorrect from statement " ++ show (j + 1)

printAnnotations (Assumption e) annotated j = do
    putStrLn $ "(" ++ show (j + 1) ++ ") " ++ show e ++ " assumption"
    if null annotated then putStrLn "Proof is correct" else printAnnotations (head annotated) (tail annotated) (j + 1)
printAnnotations Null _ _ = return ()

