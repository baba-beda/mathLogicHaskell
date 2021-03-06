module Constructions where

import qualified Data.Map as Map
import Grammar
import qualified Data.Maybe as Maybe

substitute :: Map.Map String Expr -> Expr -> Expr
substitute m (Impl a b) = Impl (substitute m a) (substitute m b)
substitute m (And a b) = And (substitute m a) (substitute m b)
substitute m (Or a b) = Or (substitute m a) (substitute m b)
substitute m (Not a) = Not (substitute m a)
substitute m (Var s) = Maybe.fromMaybe Null (Map.lookup s m)
substitute _ Null = Null

axiom1, axiom3, axiom4, axiom5, axiom6, axiom7, axiom9 :: Expr -> Expr -> Expr
axiom2, axiom8 :: Expr -> Expr -> Expr -> Expr
axiom10 :: Expr -> Expr

axiom1 a b = Impl a (Impl b a)
axiom2 a b c = Impl (Impl a b) (Impl (Impl a (Impl b c)) (Impl a c))
axiom3 a b = Impl a (Impl b (And a b))
axiom4 a b = Impl (And a b) a
axiom5 a b = Impl (And a b) b
axiom6 a b = Impl a (Or a b)
axiom7 a b = Impl b (Or a b)
axiom8 a b c = Impl (Impl a b) (Impl (Impl c b) (Impl (Or a c) b))
axiom9 a b = Impl (Impl a b) (Impl (Impl a (Not b)) (Not a))
axiom10 a = Impl (Not (Not a)) a

selfImpl :: Expr -> [Expr]
selfImpl a = [axiom1 a a, axiom1 a (Impl a a), axiom2 a (Impl a a) a, Impl (axiom1 a (Impl a a)) (Impl a a), Impl a a]
