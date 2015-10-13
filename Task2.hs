import Grammar
import Data.List.Utils
import Annotator
import Deductor

main :: IO ()
main = do
    f <- readFile "task2.in"
    let statements = lines f
    let alphaAux = head statements
    let sourceProof = map read (tail statements) :: [Expr]
    let split1 = split "|-" alphaAux
    let assumptionsAux = map read (split "," (head split1)) :: [Expr]
    let assumptions = init assumptionsAux
    let alpha = last assumptionsAux

    let res = Annotator.annotateProof assumptions  (Deductor.completeProof assumptions alpha sourceProof)
    Annotator.printAnnotations res 0

