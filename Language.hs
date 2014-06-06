module Language where

import Data.Char
import Utils

data Expr a
  =  EVar Name                     -- Variables
   | ENum Int                      -- Numbers
   | EConstr Int Int               -- Constructor tag arity
   | EAp (Expr a) (Expr a)         -- Applications
   | ELet                          -- Let(rec) expressions
        IsRec                      --   boolean with True = recursive,
        [(a, Expr a)]              --   Definitions
        (Expr a)                   --   Body of let(rec)
   | ECase                         -- Case expression
        (Expr a)                   --   Expression to scrutinise
        [Alter a]                  --   Alternatives
   | ELam [a] (Expr a)             -- Lambda abstractions
    deriving (Show)

type CoreExpr = Expr Name
type Name = String
type IsRec = Bool

recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

bindersOf :: [(a,b)] -> [a]
bindersOf defns =  [name | (name, rhs) <- defns]
rhssOf        :: [(a,b)] -> [b]
rhssOf defns  =  [rhs  | (name, rhs) <- defns]

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

type Program a = [ScDefn a]
type CoreProgram = Program Name
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

preludeDefs :: CoreProgram
preludeDefs
  = [ ("I", ["x"], EVar "x"),
      ("K", ["x","y"], EVar "x"),
      ("K1",["x","y"], EVar "y"),
      ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
                               (EAp (EVar "g") (EVar "x"))),
      ("nil", [], EConstr 1 0),
      ("cons", ["x","y"], EAp (EAp (EConstr 2 2) (EVar "x"))
                              (EVar "y")),
      ("compose", ["f","g","x"], EAp (EVar "f")
                                      (EAp (EVar "g") (EVar "x"))),
      ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ]


pprint :: CoreProgram -> String

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldll EAp e1 (take n e2s)
                    where
                    e2s = e2 : e2s

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline
iNil     = INil
iStr     = IStr
iAppend  = IAppend
iIndent  = IIndent
iNewline = INewline

iNil     :: Iseq                  -- The empty iseq
iStr     :: String -> Iseq        -- Turn a string into an iseq
iAppend  :: Iseq -> Iseq -> Iseq  -- Append two iseqs
iNewline :: Iseq                  -- New line with indentation
iIndent  :: Iseq -> Iseq          -- Indent an iseq

iDisplay :: Iseq -> String        -- Turn an iseq into a string
iDisplay seq = flatten 0 [(seq,0)]

pprDefns :: [(Name,CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
                 where
                 sep = iConcat [ iStr ";", iNewline ]
pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr)
  = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]
iConcat     :: [Iseq] -> Iseq
iInterleave :: Iseq -> [Iseq] -> Iseq
pprint prog = iDisplay (pprProgram prog)
flatten :: Int                       -- Current column; 0 for first column
            -> [(Iseq, Int)]         -- Work list
            -> String                -- Result

iNum :: Int -> Iseq
iNum n = iStr (show n)
iFWNum :: Int -> Int -> Iseq
iFWNum width n
  = iStr (space (width - length digits) ++ digits)
    where
    digits = show n
iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map lay_item (zip [1..] seqs))
             where
             lay_item (n, seq)
               = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline ]

iConcat = foldr iAppend iNil
iInterleave sep []         = iNil
iInterleave sep [seq]      = seq
iInterleave sep (seq:seqs) = seq `iAppend` (sep `iAppend` iInterleave sep seqs)
pprProgram prog = iInterleave (iAppend (iStr " ;") iNewline) (map pprSc prog)
-- Isn't this what you meant?
pprSc (name, args, body)
  = iConcat [ iStr name, iSpace, pprArgs args,
              iStr " = ", iIndent (pprExpr body) ]
pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n) = iNum n
pprExpr (EVar v) = iStr v
pprExpr (EAp (EAp (EVar "+") e1) e2)  = iConcat [pprAExpr e1, iStr " + ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "-") e1) e2)  = iConcat [pprAExpr e1, iStr " - ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "*") e1) e2)  = iConcat [pprAExpr e1, iStr " * ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "/") e1) e2)  = iConcat [pprAExpr e1, iStr " / ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "<") e1) e2)  = iConcat [pprAExpr e1, iStr " < ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "<=") e1) e2)  = iConcat [pprAExpr e1, iStr " <= ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "==") e1) e2)  = iConcat [pprAExpr e1, iStr " == ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "~=") e1) e2)  = iConcat [pprAExpr e1, iStr " ~= ", pprAExpr e2]
pprExpr (EAp (EAp (EVar ">=") e1) e2)  = iConcat [pprAExpr e1, iStr " >= ", pprAExpr e2]
pprExpr (EAp (EAp (EVar ">") e1) e2)  = iConcat [pprAExpr e1, iStr " > ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "&") e1) e2)  = iConcat [pprAExpr e1, iStr " & ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "|") e1) e2)  = iConcat [pprAExpr e1, iStr " | ", pprAExpr e2]
pprExpr (EAp e1 e2) = iConcat [ pprExpr e1, iSpace, pprAExpr e2 ]
pprExpr (ELet isrec defns expr)
  = iConcat [  iStr keyword, iNewline,
               iStr "  ", iIndent (pprDefns defns), iNewline,
               iStr "in ", pprExpr expr ]
    where
    keyword | not isrec = "let"
            | isrec = "letrec"
pprExpr (ECase e alts)
  = iConcat [  iStr "case ", pprExpr e, iStr " of", iNewline,
               iStr "  ", iIndent (iInterleave iNl (map pprAlt alts)) ]
    where
    iNl = iConcat [ iStr ";", iNewline ]
    pprAlt (tag, args, rhs)
      = iConcat [        iStr "<", iNum tag, iStr "> ",
                         pprArgs args, iStr " -> ",
                         iIndent (pprExpr rhs) ]
pprExpr (ELam args body)
  = iConcat [  iStr "(\\", pprArgs args, iStr ". ", iIndent (pprExpr body),
               iStr ")"]
pprArgs args = iInterleave iSpace (map iStr args)
pprAExpr e | isAtomicExpr e = pprExpr e
pprAExpr e | otherwise = iConcat [iStr "(", pprExpr e, iStr ")"]
flatten col ((INewline, indent) : seqs)
  = '\n' : (space indent) ++ (flatten indent seqs)
flatten col ((IIndent seq, indent) : seqs)
  = flatten col ((seq, col) : seqs)
flatten col ((IStr s, indent) : seqs)
  = s ++ flatten (col + length s) seqs
flatten col ((INil, indent) : seqs) = flatten col seqs
flatten col ((IAppend seq1 seq2, indent) : seqs)
  = flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten col [] = ""
iSpace = iStr " "


clex :: String -> [Token]
syntax :: [Token] -> CoreProgram
parse :: String -> CoreProgram
parse = syntax . clex

type Token = String           -- A token is never empty

isIdChar, isWhiteSpace :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')
isWhiteSpace c = c `elem` " \t\n"

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

type Parser a = [Token] -> [(a, [Token])]

pLit :: String -> Parser String
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)
pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
  = [ (combine v1 v2, toks2) | (v1,toks1) <- p1 toks,
                               (v2,toks2) <- p2 toks1]
pGreeting :: Parser (String, String)
pGreeting = pThen mk_pair pHelloOrGoodbye pVar
            where
            mk_pair hg name = (hg, name)
pZeroOrMore :: Parser a -> Parser [a]
pGreetings :: Parser [(String, String)]
pGreetings = pZeroOrMore pGreeting
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])
pEmpty :: a -> Parser a
pOneOrMore :: Parser a -> Parser [a]
pGreetingsN :: Parser Int
pGreetingsN = (pZeroOrMore pGreeting) `pApply` length
pApply :: Parser a -> (a -> b) -> Parser b
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pSat :: (String -> Bool) -> Parser String
pLit s = pSat (== s)
keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]
pNum :: Parser Int
syntax = take_first_parse . pProgram
         where
         take_first_parse ((prog,[]) : others) = prog
         take_first_parse (parse     : others) = take_first_parse others
         take_first_parse other                = error "Syntax error"
pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")
pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr
data PartialExpr = NoOp | FoundOp Name CoreExpr
pExpr1c :: Parser PartialExpr
pExpr1c = (pThen FoundOp (pLit "|") pExpr1) `pAlt` (pEmpty NoOp)
pExpr1 :: Parser CoreExpr
pExpr1 = pThen assembleOp pExpr2 pExpr1c
assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2
clex (c:cs) | isWhiteSpace c = clex cs
clex (c:cs) | isDigit c = num_token : clex rest_cs
             where
             num_token = c : takeWhile isDigit cs
             rest_cs   = dropWhile isDigit cs
clex (c:cs) | isAlpha c = var_tok : clex rest_cs
             where
             var_tok = c : takeWhile isIdChar cs
             rest_cs = dropWhile isIdChar cs
clex ('|':'|':cs) = clex (dropWhile (/= '\n') cs)
clex (c1:c2:cs) | ([c1, c2] `elem` twoCharOps) = [c1,c2] : clex cs
clex (c:cs) = [c] : clex cs
clex [] = []
pThen3 :: (a -> b -> c -> d)
          -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks
  = [(combine v1 v2 v3, toks3) | (v1,toks1) <- p1 toks,
                                 (v2,toks2) <- p2 toks1,
                                 (v3,toks3) <- p3 toks2]
pThen4 :: (a -> b -> c -> d -> e)
          -> Parser a -> Parser b -> Parser c
          -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 toks
  = [(combine v1 v2 v3 v4, toks4) | (v1,toks1) <- p1 toks,
                                    (v2,toks2) <- p2 toks1,
                                    (v3,toks3) <- p3 toks2,
                                    (v4,toks4) <- p4 toks3]
pEmpty v toks = [(v, toks)]
pOneOrMore p = pThen (:) p (pZeroOrMore p)
pApply p f toks = [(f v, toks') | (v, toks') <- p toks]
pOneOrMoreWithSep p psep = pThen (:) p (pOneOrMoreWithSep_c p psep)
pOneOrMoreWithSep_c p psep
  = (pThen discard_sep psep (pOneOrMoreWithSep p psep)) `pAlt`
    (pEmpty [])
    where
    discard_sep sep vs = vs
pSat pred []         = []
pSat pred (tok:toks) | pred tok = [(tok,toks)]
                     | otherwise = []
pVar = pSat isVar
       where
       isVar s = isAlpha (head s) && s `notElem` keywords
pNum = pSat (isDigit . head) `pApply` numval
numval :: String -> Int
numval = foldll (\a c -> 10 * a + ord c - ord '0') 0
mk_sc sc args eq rhs = (sc, args, rhs)
pExpr = pLet `pAlt` (pCase `pAlt` (pLambda `pAlt` pExpr1))
pLet = pThen4 mk_let
              ((pLit "let") `pAlt` (pLit "letrec")) pDefns
              (pLit "in") pExpr
       where
       mk_let keyword defns in' expr = ELet (keyword == "letrec") defns expr
pDefns = pOneOrMoreWithSep pDefn (pLit ";")
pDefn = pThen3 mk_defn pVar (pLit "=") pExpr
        where
        mk_defn var equals rhs = (var,rhs)
pCase = pThen4 mk_case (pLit "case") pExpr (pLit "of") pAlters
        where
        mk_case case' e of' alts = ECase e alts
pAlters = pOneOrMoreWithSep pAlter (pLit ";")
pAlter = pThen4 mk_alt pTag (pZeroOrMore pVar) (pLit "->") pExpr
         where
         mk_alt tag args arrow rhs = (tag, args, rhs)
pTag = pThen3 get_tag (pLit "<") pNum (pLit ">")
       where
       get_tag lb tag rb = tag
pLambda = pThen4 mk_lam
              (pLit "\\") (pOneOrMore pVar) (pLit ".") pExpr
          where
          mk_lam lam vars dot expr = ELam vars expr
pExpr2 = pThen assembleOp pExpr3 pExpr2c
pExpr2c = (pThen FoundOp (pLit "&") pExpr2) `pAlt` (pEmpty NoOp)
pExpr3 = pThen assembleOp pExpr4 pExpr3c
pExpr3c = (pThen FoundOp pRelop pExpr4) `pAlt` (pEmpty NoOp)
pRelop = pSat (`elem` relops)
         where
         relops = ["<=", "<", ">=", ">", "==", "~="]
pExpr4 = pThen assembleOp pExpr5 pExpr4c
pExpr4c = (pThen FoundOp (pLit "+") pExpr4) `pAlt`
          ((pThen FoundOp (pLit "-") pExpr5) `pAlt`
           (pEmpty NoOp))
pExpr5 = pThen assembleOp pExpr6 pExpr5c
pExpr5c = (pThen FoundOp (pLit "*") pExpr5) `pAlt`
          ((pThen FoundOp (pLit "/") pExpr6) `pAlt`
           (pEmpty NoOp))
pExpr6 = (pOneOrMore pAtomic) `pApply` mk_ap_chain
         where
         mk_ap_chain (fn:args) = foldll EAp fn args
pAtomic = pConstr `pAlt`
          (pBracExpr `pAlt`
          ((pVar `pApply` EVar) `pAlt`
          ((pNum `pApply` ENum))))
pBracExpr = pThen3 mk_brack (pLit "(") pExpr (pLit ")")
            where
            mk_brack open expr close = expr
pConstr = pThen4 pick_constr (pLit "Pack") (pLit "{") pTagArity (pLit "}")
          where
          pick_constr cons lbrack constr rbrack = constr
          pTagArity = pThen3 mk_constr pNum (pLit ",") pNum
          mk_constr tag comma arity = EConstr tag arity
