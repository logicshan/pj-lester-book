module Template1 where

import Language
import Utils

runProg :: [Char] -> [Char]
compile :: CoreProgram -> TiState
eval :: TiState -> [TiState]
showResults :: [TiState] -> [Char]
runProg = showResults . eval . compile . parse
compile program
 = (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
   where
   sc_defs = program ++ preludeDefs ++ extraPreludeDefs
   (initial_heap, globals) = buildInitialHeap sc_defs
   initial_stack = [address_of_main]
   address_of_main = aLookup globals "main" (error "main is not defined")
eval state = state : rest_states
             where
             rest_states | tiFinal state = []
                         | otherwise = eval next_state
             next_state  = doAdmin (step state)
showResults states
 = iDisplay (iConcat [ iLayn (map showState states),
                     showStats (last states)
          ])

-- The state of the machine is a quintuple whose first four components
-- correspond exactly to those given in Section 2.3.1, and whose fifth
-- component is used to accumulate statistics.
type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
-- The spine stack is just a stack of heap addresses.
type TiStack = [Addr]
data TiDump = DummyTiDump
initialTiDump = DummyTiDump
type TiHeap = Heap Node
data Node = NAp Addr Addr                     -- Application
            | NSupercomb Name [Name] CoreExpr -- Supercombinator
            | NNum Int                        -- A number
type TiGlobals = ASSOC Name Addr
tiStatInitial  :: TiStats
tiStatIncSteps :: TiStats -> TiStats
tiStatGetSteps :: TiStats -> Int
type TiStats = Int
tiStatInitial    = 0
tiStatIncSteps s = s+1
tiStatGetSteps s = s
applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats)
 = (stack, dump, heap, sc_defs, stats_fun stats)
extraPreludeDefs = []
buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap sc_defs = mapAccuml allocateSc hInitial sc_defs
allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body)
 = (heap', (name, addr))
   where
   (heap', addr) = hAlloc heap (NSupercomb name args body)
doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state
tiFinal :: TiState -> Bool
tiFinal ([sole_addr], dump, heap, globals, stats)
 = isDataNode (hLookup heap sole_addr)
tiFinal ([], dump, heap, globals, stats) = error "Empty stack!"
tiFinal state = False              -- Stack contains more than one item
isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode node     = False
step :: TiState -> TiState
step state
 = dispatch (hLookup heap (hd stack))
   where
   (stack, dump, heap, globals, stats) = state
   dispatch (NNum n)                  = numStep state n
   dispatch (NAp a1 a2)               = apStep  state a1 a2
   dispatch (NSupercomb sc args body) = scStep  state sc args body
numStep :: TiState -> Int -> TiState
numStep state n = error "Number applied as a function!"
apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2
 = (a1 : stack, dump, heap, globals, stats)
scStep   :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body
 = (new_stack, dump, new_heap, globals, stats)
   where
   new_stack = result_addr : (drop (length arg_names+1) stack)
   (new_heap, result_addr) = instantiate body heap env
   env = arg_bindings ++ globals
   arg_bindings = zip2 arg_names (getargs heap stack)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc:stack)
 = map get_arg stack
   where get_arg addr = arg  where (NAp fun arg) = hLookup heap addr
instantiate :: CoreExpr              -- Body of supercombinator
               -> TiHeap             -- Heap before instantiation
               -> ASSOC Name Addr    -- Association of names to addresses
               -> (TiHeap, Addr)     -- Heap after instantiation, and
                                     -- address of root of instance
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env
 = hAlloc heap2 (NAp a1 a2) where (heap1, a1) = instantiate e1 heap  env
                                  (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env
 = (heap, aLookup env v (error ("Undefined name " ++ show v)))
instantiate (EConstr tag arity) heap env
              = instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env
              = instantiateLet isrec defs body heap env
instantiate (ECase e alts) heap env = error "Can't instantiate case exprs"
instantiateConstr tag arity heap env
           = error "Can't instantiate constructors yet"
instantiateLet isrec defs body heap env
           = error "Can't instantiate let(rec)s yet"
showState :: TiState -> Iseq
showState (stack, dump, heap, globals, stats)
 = iConcat [ showStack heap stack, iNewline ]
showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack
 = iConcat [
       iStr "Stk [",
       iIndent (iInterleave iNewline (map show_stack_item stack)),
       iStr " ]"
   ]
   where
   show_stack_item addr
    = iConcat [ showFWAddr addr, iStr ": ",
                showStkNode heap (hLookup heap addr)
      ]
showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp fun_addr arg_addr)
 = iConcat [   iStr "NAp ", showFWAddr fun_addr,
               iStr " ", showFWAddr arg_addr, iStr " (",
               showNode (hLookup heap arg_addr), iStr ")"
   ]
showStkNode heap node = showNode node
showNode :: Node -> Iseq
showNode (NAp a1 a2) = iConcat [ iStr "NAp ", showAddr a1,
                                 iStr " ",    showAddr a2
                       ]
showNode (NSupercomb name args body) = iStr ("NSupercomb " ++ name)
showNode (NNum n) = (iStr "NNum ") `iAppend` (iNum n)
showAddr :: Addr -> Iseq
showAddr addr = iStr (show addr)
showFWAddr :: Addr -> Iseq    -- Show address in field of width 4
showFWAddr addr = iStr (space (4 -  length str) ++ str)
                  where
                  str = show addr
showStats :: TiState -> Iseq
showStats (stack, dump, heap, globals, stats)
 = iConcat [ iNewline, iNewline, iStr "Total number of steps = ",
             iNum (tiStatGetSteps stats)
   ]
