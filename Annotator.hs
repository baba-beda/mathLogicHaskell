module Annotator where

import Grammar
import Checking
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Array as Array

annotateProof :: [Expr] -> [Expr] -> [AnnotatedExpr]
annotateProof assumptions proof = do
    let firstAn = annotate assumptions proof
    if containsWrong firstAn then firstAn else annotate assumptions (removeRedundantStatements firstAn)

    

annotate :: [Expr] -> [Expr] -> [AnnotatedExpr]
annotate assumptions proof  = runST $ do
    let n = (length proof)
    proofArray <- newListArray (0, n - 1) proof :: ST s (STArray s Int Expr)
    mapProof <- newSTRef Map.empty
    neededA <- newSTRef Map.empty
    resultsMP <- newSTRef Map.empty
    auxForMsg <- newSTRef Annotator.Null
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

instance Show AnnotatedExpr where
    show (Axiom i e) = show e ++ " axiom " ++ show i
    show (MP i j e) = show e ++ " MP " ++ show (i + 1) ++ ", " ++ show (j + 1)
    show (Assumption e) = show e ++ " assumption"
    show (Wrong e) = show e
    show Annotator.Null = ""

containsWrong :: [AnnotatedExpr] -> Bool
containsWrong (expr:xs) =
    case expr of
        Wrong _ -> True
        _ -> containsWrong xs
containsWrong [] = False
removeRedundantStatements :: [AnnotatedExpr] -> [Expr]
removeRedundantStatements annotatedExprList =
    let n = length annotatedExprList in 
        let arrayAnnotated = (Array.listArray (0, n - 1) annotatedExprList :: Array.Array Int AnnotatedExpr) in
            reverse (dfs (arrayAnnotated Array.! (n - 1)) arrayAnnotated)

dfs :: AnnotatedExpr -> Array.Array Int AnnotatedExpr -> [Expr]
dfs (MP a b e) arr = [e] ++ dfs (arr Array.! a) arr ++ dfs (arr Array.! b) arr
dfs (Axiom _ e) _ = [e]
dfs (Assumption e) _  = [e]
dfs (Wrong e) _ = [e]
dfs Annotator.Null _ = []

printAnnotations :: [AnnotatedExpr] -> Int -> IO ()

printAnnotations (annE:annotated) j = do
    putStrLn $ "(" ++ show (j + 1) ++ ") " ++ show annE
    case annE of
        Wrong _ -> putStrLn $ "Proof is incorrect from statement " ++ show (j + 1)
        _ -> printAnnotations annotated (j + 1)
printAnnotations [] _ = putStrLn "Proof is correct"
