{
module Grammar where
import Data.Char
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    var     { TokenVar $$ }
    '->'    { TokenImpl }
    '&'     { TokenAnd }
    '|'     { TokenOr }
    '!'     { TokenNot }
    '('     { TokenOB }
    ')'     { TokenCB }

%%

Expr        : Or                { RuleOr1 $1 }
            | Or '->' Expr      { RuleImpl $1 $3 }

Or          : And               { RuleAnd1 $1 }
            | Or '|' And        { RuleOr $1 $3 }

And         : Not               { RuleNot1 $1 }
            | And '&' Not       { RuleAnd $1 $3 }

Atom        : Var               { RuleVar1 $1 }
            | '(' Expr ')'      { RuleBrack $2 }

Not         : '!' Atom          { RuleNotGen $2 }
            | Atom              { RuleAtom1 $1 }

Var         : var               { RuleVar $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse Error"

data Token
        = TokenVar String
        | TokenImpl
        | TokenAnd
        | TokenOr
        | TokenNot
        | TokenOB
        | TokenCB
        deriving Show

data RuleExpr
    = RuleOr1 RuleOr
    | RuleImpl RuleOr RuleExpr

data RuleOr
    = RuleAnd1 RuleAnd
    | RuleOr RuleOr RuleAnd

data RuleAnd
    = RuleNot1 RuleNot
    | RuleAnd RuleAnd RuleNot

data RuleNot
    = RuleNotGen RuleAtom
    | RuleAtom1 RuleAtom

data RuleAtom
    = RuleVar1 RuleVar
    | RuleBrack RuleExpr

data RuleVar
    = RuleVar String
 
data Expr
        = Var String
        | Not Expr
        | Impl Expr Expr
        | Or Expr Expr
        | And Expr Expr
        deriving (Eq)

atomToExpr :: RuleAtom -> Expr
atomToExpr (RuleVar1 (RuleVar v)) = Var v
atomToExpr (RuleBrack expr) = ruleToExpr expr

orToExpr :: RuleOr -> Expr
orToExpr (RuleAnd1 and) = andToExpr and
orToExpr (RuleOr or and) = Or (orToExpr or) (andToExpr and)

andToExpr :: RuleAnd -> Expr
andToExpr (RuleNot1 not) = notToExpr not
andToExpr (RuleAnd and not) = And (andToExpr and) (notToExpr not)

notToExpr :: RuleNot -> Expr
notToExpr (RuleNotGen not) = Not (atomToExpr not)
notToExpr (RuleAtom1 atom) = atomToExpr atom

ruleToExpr :: RuleExpr -> Expr
ruleToExpr (RuleImpl or expr) = Impl (orToExpr or) (ruleToExpr expr)
ruleToExpr (RuleOr1 or) = orToExpr or

instance Read Expr where
    readsPrec _ s = [(ruleToExpr $ parse $ lexer $ dropWhile isSpace s, "")]

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
    | isSpace c = lexer cs
    | isAlpha c = TokenVar var : lexer rest
        where var = c:ds
              (ds, rest) = span isDigit cs
lexer ('-':'>':cs) = TokenImpl : lexer cs
lexer ('&':cs) = TokenAnd : lexer cs
lexer ('|':cs) = TokenOr : lexer cs
lexer ('!':cs) = TokenNot : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
}
