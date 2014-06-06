{-# LANGUAGE NPlusKPatterns #-}
module Gm6 where

import Language
import Utils
import Data.List (isPrefixOf)

runProg :: [Char] -> [Char]
runProg = showResults . eval . compile . parse

type GmCode = [Instruction]
getCode :: GmState -> GmCode
putCode :: GmCode -> GmState -> GmState
type GmStack = [Addr]
getStack :: GmState -> GmStack
putStack :: GmStack -> GmState -> GmState
type GmHeap = Heap Node
getHeap :: GmState -> GmHeap
putHeap :: GmHeap -> GmState -> GmState
type GmGlobals = ASSOC Name Addr
getGlobals :: GmState -> GmGlobals
statInitial  :: GmStats
statIncSteps :: GmStats -> GmStats
statGetSteps :: GmStats -> Int
type GmStats = Int
statInitial    = 0
statIncSteps s = s+1
statGetSteps s = s
getStats :: GmState -> GmStats
putStats :: GmStats -> GmState -> GmState
eval :: GmState -> [GmState]
eval state = state: restStates
             where
             restStates | gmFinal state     = []
                        | otherwise         = eval nextState
             nextState  = doAdmin (step state)
doAdmin :: GmState -> GmState
doAdmin s = putStats (statIncSteps (getStats s)) s
gmFinal :: GmState -> Bool
gmFinal s = case (getCode s) of
                   []        -> True
                   otherwise -> False
step :: GmState -> GmState
step state = dispatch i (putCode is state)
             where (i:is) = getCode state
pushglobal :: Name -> GmState -> GmState
pushglobal f state
  | (isPrefixOf "Pack " f) && (not $ f `elem` (aDomain $ getGlobals state))
    = putGlobals ((f,a'):getGlobals state) (putHeap heap' (putStack (a': getStack state) state))
  | otherwise = putStack (a: getStack state) state
  where a = aLookup (getGlobals state) f (error ("Undeclared global " ++ f))
        (heap', a') = hAlloc (getHeap state) (NGlobal n [Pack t n, Update 0, Unwind])
        [_, st, sn] = words f
        t = read st :: Int
        n = read sn :: Int
  
pushint :: Int -> GmState -> GmState
pushint n state
      = putHeap heap' (putStack (a: getStack state) state)
      where (heap', a) = hAlloc (getHeap state) (NNum n)
mkap :: GmState -> GmState
mkap state
      = putHeap heap' (putStack (a:as') state)
      where (heap', a)  = hAlloc (getHeap state) (NAp a1 a2)
            (a1:a2:as') = getStack state
getArg :: Node -> Addr
getArg (NAp a1 a2) = a2
slide :: Int -> GmState -> GmState
slide n state
      = putStack (a: drop n as) state
      where (a:as) = getStack state
buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitialHeap program
   = mapAccuml allocateSc hInitial compiled
   where compiled = map compileSc (preludeDefs ++ program) ++
                    compiledPrimitives
type GmCompiledSC = (Name, Int, GmCode)
allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns)
      = (heap', (name, addr))
      where (heap', addr) = hAlloc heap (NGlobal nargs instns)
compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body)
      = (name, length env, compileR body (zip2 env [0..]))
compileR e env = compileE e env ++ [Slide (length env + 1), Unwind]
type GmCompiler = CoreExpr -> GmEnvironment -> GmCode
type GmEnvironment = ASSOC Name Int
argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, n+m) | (v,m) <- env]
showResults :: [GmState] -> [Char]
showResults states
      = iDisplay (iConcat [
      iStr "Supercombinator definitions", iNewline,
      iInterleave iNewline (map (showSC s) (getGlobals s)),
      iNewline, iNewline, iStr "State transitions", iNewline, iNewline,
      iLayn (map showState states),
      iNewline, iNewline,
      showStats (last states)])
      where (s:ss) = states
showSC :: GmState -> (Name, Addr) -> Iseq
showSC s (name, addr)
      = iConcat [ iStr "Code for ", iStr name, iNewline,
            showInstructions code, iNewline, iNewline]
      where (NGlobal arity code) = (hLookup (getHeap s) addr)
showInstructions :: GmCode -> Iseq
showInstructions is
      = iConcat [iStr "  Code:{",
           iIndent (iInterleave iNewline (map showInstruction is)),
           iStr "}", iNewline]
showStack :: GmState -> Iseq
showStack s
      = iConcat [iStr " Stack:[",
           iIndent (iInterleave iNewline
                       (map (showStackItem s) (reverse (getStack s)))),
           iStr "]"]
showStackItem :: GmState -> Addr -> Iseq
showStackItem s a
      = iConcat [iStr (showaddr a), iStr ": ",
           showNode s a (hLookup (getHeap s) a)]
showStats :: GmState -> Iseq
showStats s
      = iConcat [ iStr "Steps taken = ", iNum (statGetSteps (getStats s))]
rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap as
      = take n as' ++ drop n as
      where as' = map (getArg . hLookup heap) (tl as)
allocNodes :: Int -> GmHeap -> (GmHeap, [Addr])
allocNodes 0     heap = (heap,  [])
allocNodes (n+1) heap = (heap2, a:as)
                        where (heap1, as) = allocNodes n heap
                              (heap2, a)  = hAlloc heap1 (NInd hNull)
compileLet :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLet comp defs expr env
  = compileLet' defs env ++ comp expr env' ++ [Slide (length defs)]
    where env' = compileArgs defs env
compileLet' :: [(Name, CoreExpr)] -> GmEnvironment -> GmCode
compileLet' []                  env = []
compileLet' ((name, expr):defs) env
    = compileC expr env ++ compileLet' defs (argOffset 1 env)
compileArgs :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
compileArgs defs env
    = zip (map first defs) [n-1, n-2 .. 0] ++ argOffset n env
            where n = length defs
type GmDump = [GmDumpItem]
type GmDumpItem = (GmCode, GmStack)
getDump :: GmState -> GmDump
putDump :: GmDump -> GmState -> GmState
showDump :: GmState -> Iseq
showDump s
    = iConcat       [iStr "  Dump:[",
                     iIndent (iInterleave iNewline
                            (map showDumpItem (reverse (getDump s)))),
                     iStr "]"]
showDumpItem :: GmDumpItem -> Iseq
showDumpItem (code, stack)
    = iConcat       [iStr "<",
                     shortShowInstructions 3 code, iStr ", ",
                     shortShowStack stack,         iStr ">"]
shortShowInstructions :: Int -> GmCode -> Iseq
shortShowInstructions number code
    = iConcat [iStr "{", iInterleave (iStr "; ") dotcodes, iStr "}"]
    where   codes   = map showInstruction (take number code)
            dotcodes      | length code > number  = codes ++ [iStr "..."]
                          | otherwise             = codes
shortShowStack :: GmStack -> Iseq
shortShowStack stack
    = iConcat [iStr "[",
           iInterleave (iStr ", ") (map (iStr . showaddr) stack),
           iStr "]"]
boxInteger :: Int -> GmState -> GmState
boxInteger n state
      = putStack (a: getStack state) (putHeap h' state)
      where (h', a) = hAlloc (getHeap state) (NNum n)
unboxInteger :: Addr -> GmState -> Int
unboxInteger a state
      = ub (hLookup (getHeap state) a)
      where   ub (NNum i) = i
              ub n        = error "Unboxing a non-integer"
primitive1 :: (b -> GmState -> GmState)  -- boxing function
         -> (Addr -> GmState -> a)       -- unbixing function
         -> (a -> b)                     -- operator
         -> (GmState -> GmState)         -- state transition
primitive1 box unbox op state
   = box (op (unbox a state)) (putStack as state)
   where (a:as) = getStack state
primitive2 :: (b -> GmState -> GmState)  -- boxing function
         -> (Addr -> GmState -> a)       -- unbixing function
         -> (a -> a -> b)                -- operator
         -> (GmState -> GmState)         -- state transition
primitive2 box unbox op state
   = box (op (unbox a0 state) (unbox a1 state)) (putStack as state)
   where (a0:a1:as) = getStack state
arithmetic1 ::   (Int -> Int)            -- arithmetic operator
                 -> (GmState -> GmState) -- state transition
arithmetic1 = primitive1 boxInteger unboxInteger
arithmetic2 ::   (Int -> Int -> Int)     -- arithmetic operation
                 -> (GmState -> GmState) -- state transition
arithmetic2 = primitive2 boxInteger unboxInteger
comparison :: (Int -> Int -> Bool) -> GmState -> GmState
comparison = primitive2 boxBoolean unboxInteger
compiledPrimitives :: [GmCompiledSC]
compiledPrimitives
   =       [("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind]),
            ("-", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind]),
            ("*", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind]),
            ("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind]),
            ("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind]),
            ("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind]),
            ("~=", 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind]),
            ("<",  2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind]),
            ("<=", 2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind]),
            (">",  2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind]),
            (">=", 2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind]),
            ("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2],
                            Update 3, Pop 3, Unwind])]
builtInDyadic :: ASSOC Name Instruction
builtInDyadic
    =       [("+", Add), ("-", Sub), ("*", Mul), ("div", Div),
             ("==", Eq), ("~=", Ne), (">=", Ge),
             (">",  Gt), ("<=", Le), ("<",  Lt)]
type GmState =
   (GmOutput,              -- Current Output
    GmCode,                -- Current Instruction Stream
    GmStack,               -- Current Stack
    GmDump,                -- The Dump
    GmHeap,                -- Heap of Nodes
    GmGlobals,             -- Global addresses in Heap
    GmStats)               -- Statistics
type GmOutput = [Char]
getOutput :: GmState -> GmOutput
getOutput (o, i, stack, dump, heap, globals, stats) = o
putOutput :: GmOutput -> GmState -> GmState
putOutput o' (o, i, stack, dump, heap, globals, stats)
     = (o', i, stack, dump, heap, globals, stats)
data Node
          = NNum Int              -- Numbers
          | NAp Addr Addr         -- Applications
          | NGlobal Int GmCode    -- Globals
          | NInd Addr
          | NConstr Int [Addr]

instance Eq Node
  where
  NNum a       == NNum b          = a == b    -- needed to check conditions
  NAp a b      == NAp c d         = False     -- not needed
  NGlobal a b  == NGlobal c d     = False     -- not needed
  NInd a       == NInd b          = False     -- not needed
  NConstr a b  == NConstr c d     = a == c && b == d
  --NConstr a b  == NConstr c d     = False     -- not needed
  _            == _               = False

showState :: GmState -> Iseq
showState s
     = iConcat [showOutput s,                 iNewline,
             showStack s,                  iNewline,
             showDump s,                   iNewline,
             showInstructions (getCode s), iNewline]
showOutput :: GmState -> Iseq
showOutput s = iConcat [iStr "Output:\"", iStr (getOutput s), iStr "\""]
showNode :: GmState -> Addr -> Node -> Iseq
showNode s a (NNum n)      = iNum n
showNode s a (NGlobal n g) = iConcat [iStr "Global ", iStr v]
  where v = head [n | (n,b) <- getGlobals s, a==b]
showNode s a (NAp a1 a2)   = iConcat [iStr "Ap ",  iStr (showaddr a1),
                                      iStr " ",    iStr (showaddr a2)]
showNode s a (NInd a1)     = iConcat [iStr "Ind ", iStr (showaddr a1)]
showNode s a (NConstr t as)
 = iConcat [iStr "Cons ", iNum t, iStr " [",
            iInterleave (iStr ", ") (map (iStr.showaddr) as), iStr "]"]
data Instruction 
   = Slide Int
   | Alloc Int
   | Update Int
   | Pop Int
   | Unwind
   | Pushglobal Name
   | Pushint Int
   | Push Int
   | Mkap
   | Eval
   | Add | Sub | Mul | Div
   | Neg
   | Eq | Ne | Lt | Le | Gt | Ge
   | Cond GmCode GmCode
   | Pack Int Int
   | Casejump [(Int, GmCode)]
   | Split Int
   | Print

compileAlts ::    (Int -> GmCompiler)     -- compiler for alternative bodies
                  -> [CoreAlt]            -- the list of alternatives
                  -> GmEnvironment        -- the current environment
                  -> [(Int, GmCode)]      -- list of alternative code sequences
compileAlts comp alts env
 = [(tag, comp (length names) body (zip names [0..] ++ argOffset (length names) env))
         | (tag, names, body) <- alts]
compileE' :: Int -> GmCompiler
compileE' offset expr env
    = [Split offset] ++ compileE expr env ++ [Slide offset]
boxBoolean :: Bool -> GmState -> GmState
boxBoolean b state
 = putStack (a: getStack state) (putHeap h' state)
   where (h',a) = hAlloc (getHeap state) (NConstr b' [])
         b' | b = 2              -- 2 is tag of True
            | otherwise = 1      -- 1 is tag of False
update :: Int -> GmState -> GmState
update n state
 = putHeap heap' (putStack as state)
   where heap'  = hUpdate (getHeap state) (as !! n) (NInd a)
         (a:as) = getStack state
pop :: Int -> GmState -> GmState
pop n state
 = putStack (drop n (getStack state)) state
compileLetrec comp defs e args
 = [Alloc n]           ++
   compiled defs (n-1) ++
   comp e newArgs      ++
   [Slide n]
   where newArgs = compileArgs defs args
         n       = length defs
         compiled []     i = []
         compiled (d:ds) i = compileC (second d) newArgs ++
                             [Update i]                  ++
                             compiled ds (i-1)
alloc :: Int -> GmState -> GmState
alloc n state
 = putHeap heap' (putStack (as'++getStack state) state)
   where (heap', as') = allocNodes n (getHeap state)
push :: Int -> GmState -> GmState
push n state
 = putStack ((as !! n): as) state
   where as = getStack state
cond :: GmCode -> GmCode -> GmState -> GmState
cond i1 i2 state
 = putCode (i'++i) (putStack s state)
   where (a:s) = getStack state
         i' | hLookup (getHeap state) a == (NConstr 2 []) = i1     -- Pack{2,0} means true
            | otherwise                                   = i2     -- Pack{1,0} means false
         i     = getCode state
evalop :: GmState -> GmState
evalop state
 = putCode [Unwind] (putStack [a] (putDump d' state))
   where (a:s) = getStack state
         d'    = (getCode state, s): getDump state
showInstruction (Slide n)      = (iStr "Slide ")      `iAppend` (iNum n)
showInstruction (Alloc n)      = (iStr "Alloc ")      `iAppend` (iNum n)
showInstruction (Update n)     = (iStr "Update ")     `iAppend` (iNum n)
showInstruction (Pop n)        = (iStr "Pop ")        `iAppend` (iNum n)
showInstruction Unwind         = iStr  "Unwind"
showInstruction (Pushglobal f) = (iStr "Pushglobal ") `iAppend` (iStr f)
showInstruction (Pushint n)    = (iStr "Pushint ")    `iAppend` (iNum n)
showInstruction (Push n)       = (iStr "Push ")       `iAppend` (iNum n)
showInstruction Mkap           = iStr  "Mkap"
showInstruction Eval           = iStr  "Eval"
showInstruction Add            = iStr  "Add"
showInstruction Sub            = iStr  "Sub"
showInstruction Mul            = iStr  "Mul"
showInstruction Div            = iStr  "Div"
showInstruction Neg            = iStr  "Neg"
showInstruction Eq             = iStr  "Eq"
showInstruction Ne             = iStr  "Ne"
showInstruction Le             = iStr  "Le"
showInstruction Lt             = iStr  "Lt"
showInstruction Ge             = iStr  "Ge"
showInstruction Gt             = iStr  "Gt"
showInstruction (Pack t a)     = ((iStr "Pack ")   `iAppend` (iNum t))
                                 `iAppend` (iNum a)
showInstruction (Casejump nis) = (iStr "Casejump ")   `iAppend`
                                 (showAlternatives nis)
showInstruction (Split n)      = (iStr "Split ")      `iAppend` (iNum n)
showInstruction Print          = iStr "Print"
showInstruction (Cond i1 i2)
 = iConcat [iStr "Cond [2: ", shortShowInstructions 2 i1,
            iStr ", 1: ",     shortShowInstructions 2 i2, iStr "]"] 
showAlternatives nis
 = iConcat [iStr "[",
            iInterleave (iStr ", ") (map showLabelInstructions nis),
            iStr "]"]
   where showLabelInstructions (tag, code)
          = iConcat [iNum tag, iStr ": ", shortShowInstructions 2 code]
dispatch :: Instruction -> GmState -> GmState
dispatch (Slide n)      = slide n
dispatch Unwind         = unwind
dispatch (Pushglobal f) = pushglobal f
dispatch (Push n)       = push n
dispatch (Pushint n)    = pushint n
dispatch Mkap           = mkap
dispatch (Update n)     = update n
dispatch (Pop n)        = pop n
dispatch (Alloc n)      = alloc n
dispatch Add            = arithmetic2 (+)
dispatch Sub            = arithmetic2 (-)
dispatch Mul            = arithmetic2 (*)
dispatch Div            = arithmetic2 (div)
dispatch Neg            = arithmetic1 negate
dispatch Eq             = comparison (==)
dispatch Ne             = comparison (/=)
dispatch Lt             = comparison (<)
dispatch Le             = comparison (<=)
dispatch Gt             = comparison (>)
dispatch Ge             = comparison (>=)
dispatch Eval           = evalop
dispatch (Cond i1 i2)   = cond i1 i2 
dispatch (Casejump alts)= casejump alts
dispatch (Split n)      = split n
dispatch Print          = gmprint
dispatch (Pack t a)     = pack t a
casejump alts state
 = putCode (i ++ getCode state) state
   where (NConstr t as) = hLookup (getHeap state) (hd (getStack state))
         i = aLookup alts t (error ("No case for constructor" ++ show t))
split :: Int -> GmState -> GmState
split j state
 = putStack (as++s) state
   where (NConstr t as) = hLookup (getHeap state) a
         (a:s)          = getStack state
gmprint :: GmState -> GmState  -- gmprint not print, to avoid name clash
gmprint state
 = newState (hLookup (getHeap state) a) state
   where
   newState (NConstr t as) = putOutput ("Pack{"++show t++","++ show(length as)++"}"). -- KH
                            putCode (printcode (length as) ++ getCode state) .
                             putStack (as++s)
   newState (NNum n)       = putOutput (show n) . putStack s
   newState n              = error "Print of non data structure"
   (a:s) = getStack state
printcode 0     = []
printcode (n+1) = Eval: Print: printcode n
pack :: Int -> Int -> GmState -> GmState
pack t a state
 = putHeap heap' (putStack (addr: drop a s) state)
   where s = getStack state
         (heap', addr) = hAlloc (getHeap state) (NConstr t (take a s))
unwind :: GmState -> GmState
unwind state
 = newState (hLookup heap a)
   where
   (a:as)       = getStack state
   heap         = getHeap state
   ((i',s'):d') = getDump state
   newState (NNum n)
    = putCode i' (putStack (a:s') (putDump d' state))
   newState (NAp a1 a2)
    = putCode [Unwind] (putStack (a1:a:as) state)
   newState (NGlobal n c)
    | length as >= n = putCode c  (putStack rs state)
    | otherwise      = putCode i' (putStack (last (a:as):s') (putDump d' state))
      where rs = rearrange n heap (a:as)
   newState (NInd a1)
    = putCode [Unwind] (putStack (a1:as) state)
   newState (NConstr t as)
    = putCode i' (putStack (a:s') (putDump d' state))
getCode (o, i, stack, dump, heap, globals, stats) = i
putCode i' (o, i, stack, dump, heap, globals, stats)
 = (o, i', stack, dump, heap, globals, stats)
getStack (o, i, stack, dump, heap, globals, stats) = stack
putStack stack' (o, i, stack, dump, heap, globals, stats)
 = (o, i, stack', dump, heap, globals, stats)
getDump (o, i, stack, dump, heap, globals, stats) = dump
putDump dump' (o, i, stack, dump, heap, globals, stats)
 = (o, i, stack, dump', heap, globals, stats)
getHeap (o, i, stack, dump, heap, globals, stats) = heap
putHeap heap' (o, i, stack, dump, heap, globals, stats)
 = (o, i, stack, dump, heap', globals, stats)
getGlobals (o, i, stack, dump, heap, globals, stats) = globals
putGlobals globals' (o, i, stack, dump, heap, globals, stats) -- KH for exercise
 = (o, i, stack, dump, heap, globals', stats)
getStats (o, i, stack, dump, heap, globals, stats) = stats
putStats stats' (o, i, stack, dump, heap, globals, stats)
 = (o, i, stack, dump, heap, globals, stats')
compile :: CoreProgram -> GmState
compile program
 = ([], initialCode, [], [], heap, globals, statInitial)
   where (heap, globals) = buildInitialHeap program
initialCode :: GmCode
initialCode = [Pushglobal "main", Eval, Print]
compileC :: GmCompiler
compileC (EConstr t 0) args = [Pack t 0]
compileC (EConstr t a) args = [Pushglobal $ "Pack " ++ show t ++ " " ++ show a]
compileC (EVar v)      args | v `elem` (aDomain args) = [Push n]
                            | otherwise = [Pushglobal v]
                              where n = aLookup args v (error "")
compileC (ENum n)      args = [Pushint n]
compileC (ELet recursive defs e)
                       args | recursive = compileLetrec compileC defs e args
                            | otherwise = compileLet    compileC defs e args

compileC (EAp e1 e2)   args
 | saturatedCons spine = compileCS (reverse spine) args
 | otherwise = compileC e2 args ++ compileC e1 (argOffset 1 args) ++ [Mkap]
   where spine = makeSpine (EAp e1 e2)
         saturatedCons (EConstr t a:es) = a == length es
         saturatedCons (e:es)           = False

makeSpine (EAp e1 e2) = makeSpine e1 ++ [e2]
makeSpine e           = [e]
compileCS [EConstr t a] args = [Pack t a]
compileCS (e:es)        args = compileC  e args ++
                               compileCS es (argOffset 1 args)
compileE :: GmCompiler
compileE (EAp (EAp (EVar op) e1) e2) args
 | op `elem` binaryOps = compileE e2 args ++ compileE e1 args' ++ [inst]
   where binaryOps = map first builtInDyadic
         inst = aLookup builtInDyadic op (error "This can't happen")
         args' = argOffset 1 args
compileE (EAp (EVar "negate") e) args
 = compileE e args ++ [Neg]
compileE (EAp (EAp (EAp (EVar "if") e1) e2) e3) args
 = compileE e1 args ++ [Cond (compileE e2 args) (compileE e3 args)]
compileE (ENum n)      args = [Pushint n]
compileE (ELet recursive defs e)
                       args | recursive = compileLetrec compileE defs e args
                            | otherwise = compileLet    compileE defs e args
compileE (ECase e as)  args = compileE e args ++
                              [Casejump (compileAlts compileE' as args)]
compileE e             args = compileC e args ++ [Eval]
strictOperators :: ASSOC Name (Instruction, Int)
strictOperators
 = [("+", (Add, 2)), ("-", (Sub, 2)), ("*", (Mul, 2)), ("/", (Div, 2)),
    ("negate", (Neg, 1)),
    ("==", (Eq, 2)), ("~=", (Ne, 2)), (">=", (Ge, 2)),
    (">",  (Gt, 2)), ("<=", (Le, 2)), ("<",  (Lt, 2))]
