module Deductor where

import Grammar
import Checking
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import Constructions
import qualified Data.Map as Map

completeProof :: [Expr] -> Expr -> [Expr] -> [Expr]
completeProof assumptions alpha sourceProof = runST $ do
    let n = length sourceProof
    sourceProofArray <- newListArray (0, n - 1) sourceProof :: ST s (STArray s Int Expr)
    mapSourceProof <- newSTRef Map.empty
    neededA <- newSTRef Map.empty
    resultsMP <- newSTRef Map.empty
    proof <- newSTRef []
    assumptionsSet <- newSTRef (Set.fromList assumptions)
    forM_ [0..n - 1] $ \i -> do
        mSPr <- readSTRef mapSourceProof
        nA <- readSTRef neededA
        resMP <- readSTRef resultsMP
        curExp <- readArray sourceProofArray i
        asmpt <- readSTRef assumptionsSet
        pr <- readSTRef proof
        let a = isAxiom curExp
        if a > 0 || Set.member curExp asmpt then
                writeSTRef proof (pr ++ [curExp, axiom1 curExp alpha, Impl alpha curExp])
        else
            if curExp == alpha then
                writeSTRef proof (pr ++ selfImpl alpha)
            else
                case Map.lookup curExp resMP of
                    Nothing -> writeSTRef proof (pr ++ [curExp])
                    Just (f, s) -> do
                        deltaJ <- readArray sourceProofArray f
                        deltaK <- readArray sourceProofArray s
                        writeSTRef proof (pr ++ [axiom2 alpha deltaJ curExp, Impl (Impl alpha deltaK) (Impl alpha curExp), Impl alpha curExp])
        case curExp of
            (Impl al beta) ->
                case Map.lookup al mSPr of
                    Nothing -> writeSTRef neededA (Map.insert al i nA)
                    Just num -> writeSTRef resultsMP (Map.insert beta (num, i) resMP)
            _ -> return ()
        case Map.lookup curExp nA of
            Nothing -> return ()
            Just ind -> do
                Impl _ beta <- readArray sourceProofArray ind
                writeSTRef resultsMP (Map.insert beta (i, ind) resMP)
                writeSTRef neededA (Map.delete curExp nA)
        writeSTRef mapSourceProof (Map.insert curExp i mSPr)
    readSTRef proof
        

