{-# LANGUAGE NPlusKPatterns #-}
module Gm5 where
import Language
import Utils
-- The function run is already defined in gofers standard.prelude
runProg :: [Char] -> [Char]
runProg = showResults . eval . compile . parse
-- :a language.lhs -- parser data types
-- :a util.lhs -- heap data type and other library functions
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
      = putStack (a: getStack state) state
      where a = aLookup (getGlobals state) f (error ("Undeclared global " ++ f))
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
data Node 
 = NNum Int              -- Numbers
 | NAp Addr Addr         -- Applications
 | NGlobal Int GmCode    -- Globals
 | NInd Addr             -- Indirections
instance Eq Node
  where
  NNum a       == NNum b          = a == b    -- needed to check conditions
  NAp a b      == NAp c d         = False     -- not needed
  NGlobal a b  == NGlobal c d     = False     -- not needed
  NInd a       == NInd b          = False     -- not needed
rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap as
      = take n as' ++ drop n as
      where as' = map (getArg . hLookup heap) (tl as)
allocNodes :: Int -> GmHeap -> (GmHeap, [Addr])
allocNodes 0     heap = (heap,  [])
allocNodes (n+1) heap = (heap2, a:as)
                        where (heap1, as) = allocNodes n heap
                              (heap2, a)  = hAlloc heap1 (NInd hNull)
compileC :: GmCompiler
compileC (EVar v)      args 
 | elem v (aDomain args) = [Push n]
 | otherwise             = [Pushglobal v]
         where n = aLookup args v (error "")
compileC (ENum n)    env = [Pushint n]
compileC (EAp e1 e2) env = compileC e2 env ++
                           compileC e1 (argOffset 1 env) ++
                           [Mkap]
compileC (ELet recursive defs e) args 
         | recursive     = compileLetrec compileC defs e args
         | otherwise     = compileLet    compileC defs e args
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
type GmState = ( GmCode,              -- current Instruction
                GmStack,      -- current Stack
                GmDump,       -- current Dump
                GmHeap,       -- Heap of Nodes
                GmGlobals,    -- Global adresses in Heap
                GmStats)      -- Statistics
type GmDump = [GmDumpItem]
type GmDumpItem = (GmCode, GmStack)
getDump :: GmState -> GmDump
getDump (i, stack, dump, heap, globals, stats) = dump
putDump :: GmDump -> GmState -> GmState
putDump dump' (i, stack, dump, heap, globals, stats)
   = (i, stack, dump', heap, globals, stats)
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
           | Add | Sub | Mul | Div | Neg
           | Eq | Ne | Lt | Le | Gt | Ge
         | Cond GmCode GmCode
showState :: GmState -> Iseq
showState s
   = iConcat       [showStack s,                  iNewline,
                    showDump s,                   iNewline,
                    showInstructions (getCode s), iNewline]
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
boxBoolean :: Bool -> GmState -> GmState
boxBoolean b state
   = putStack (a: getStack state) (putHeap h' state)
   where   (h',a) = hAlloc (getHeap state) (NNum b')
           b' | b                = 1
              | otherwise        = 0
comparison :: (Int -> Int -> Bool) -> GmState -> GmState
comparison = primitive2 boxBoolean unboxInteger
compile :: CoreProgram -> GmState
compile program
   = (initialCode, [], [], heap, globals, statInitial)
   where (heap, globals) = buildInitialHeap program
initialCode :: GmCode
initialCode = [Pushglobal "main", Eval]
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
showNode s a (NNum n)      = iNum n
showNode s a (NGlobal n g) = iConcat [iStr "Global ", iStr v]
                             where v = hd [n | (n,b) <- globals, a==b]
                                   globals = getGlobals s
showNode s a (NAp a1 a2)   = iConcat [iStr "Ap ",  iStr (showaddr a1),
                                      iStr " ",    iStr (showaddr a2)]
showNode s a (NInd a1)     = iConcat [iStr "Ind ", iStr (showaddr a1)]
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
getCode (i, stack, dump, heap, globals, stats) = i
putCode i' (i, stack, dump, heap, globals, stats)
 = (i', stack, dump, heap, globals, stats)
getStack (i, stack, dump, heap, globals, stats) = stack
putStack stack' (i, stack, dump, heap, globals, stats)
 = (i, stack', dump, heap, globals, stats)
getHeap (i, stack, dump, heap, globals, stats) = heap
putHeap heap' (i, stack, dump, heap, globals, stats)
 = (i, stack, dump, heap', globals, stats)
getGlobals (i, stack, dump, heap, globals, stats) = globals
getStats (i, stack, dump, heap, globals, stats) = stats
putStats stats' (i, stack, dump, heap, globals, stats)
 = (i, stack, dump, heap, globals, stats')
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
showInstruction Lt             = iStr  "Lt"
showInstruction Le             = iStr  "Le"
showInstruction Gt             = iStr  "Gt"
showInstruction Ge             = iStr  "Ge"
showInstruction (Cond i1 i2)
 = iConcat [iStr "Cond [2: ", shortShowInstructions 2 i1,
            iStr ", 1: ",     shortShowInstructions 2 i2, iStr "]"] 
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
cond :: GmCode -> GmCode -> GmState -> GmState
cond i1 i2 state
 = putCode (i'++i) (putStack s state)
   where (a:s) = getStack state
         i' | hLookup (getHeap state) a == NNum 1 = i1     -- 1 means true
            | otherwise                           = i2     -- 0 means false
         i     = getCode state
evalop :: GmState -> GmState
evalop state
 = putCode [Unwind] (putStack [a] (putDump d' state))
   where (a:s) = getStack state
         d'    = (getCode state, s): getDump state
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
    | otherwise  =  putCode i' (putStack (last (a:as):s') (putDump d' state))
    where rs = rearrange n heap (a:as)
   newState (NInd a1)
    = putCode [Unwind] (putStack (a1:as) state)
compileR :: GmCompiler
compileR (EAp (EAp (EVar op) e1) e2) args
 | op `elem` binaryOps = compileE (EAp (EAp (EVar op) e1) e2) args ++
                           [Update n, Pop n, Unwind]
   where binaryOps = map first builtInDyadic
         n =  length args
compileR (EAp (EVar "negate") e) args
 = compileE (EAp (EVar "negate") e) args ++ [Update n, Pop n, Unwind]
   where n = length args
compileR (EAp (EAp (EAp (EVar "if") e1) e2) e3) args
  = compileE e1 args ++ [Cond (compileR e2 args) (compileR e3 args)]
compileR e args = compileC e args ++ [Update n, Pop n, Unwind]
                   where n = length args
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
compileE e             args = compileC e args ++ [Eval]
