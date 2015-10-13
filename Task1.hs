import Grammar
import Annotator
main :: IO ()
main = do
    f <- readFile "task1.in"
    let proof = map read (lines f) :: [Expr]
    let res = Annotator.annotateProof [] proof
    Annotator.printAnnotations res 0
