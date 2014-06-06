module Template5 where

import Language
import Utils

runProg :: [Char] -> [Char]
compile :: CoreProgram -> TiState
eval :: TiState -> [TiState]
showResults :: [TiState] -> [Char]
runProg = showResults . eval . compile . parse

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
type TiStack = [Addr]
type TiHeap = Heap Node
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
compile program
 = (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
   where
   sc_defs = program ++ preludeDefs ++ extraPreludeDefs
   (initial_heap, globals) = buildInitialHeap sc_defs
   initial_stack = [address_of_main]
   address_of_main = aLookup globals "main" (error "main is not defined")
allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body)
 = (heap', (name, addr))
   where
   (heap', addr) = hAlloc heap (NSupercomb name args body)
eval state = state : rest_states
             where
             rest_states | tiFinal state = []
                         | otherwise = eval next_state
             next_state  = doAdmin (step state)
doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state
step :: TiState -> TiState

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
showResults states
 = iDisplay (iConcat [ iLayn (map showState states),
                     showStats (last states)
          ])
showState :: TiState -> Iseq
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
instantiateAndUpdate 
    :: CoreExpr             -- Body of supercombinator
       -> Addr              -- Address of node to update
       -> TiHeap            -- Heap before instantiation
       -> ASSOC Name Addr   -- Associate parameters to addresses
       -> TiHeap            -- Heap after instantiation
type TiDump = [TiStack]
initialTiDump = []
buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap sc_defs
 = (heap2, sc_addrs ++ prim_addrs)
   where
   (heap1, sc_addrs)   = mapAccuml allocateSc hInitial sc_defs
   (heap2, prim_addrs) = mapAccuml allocatePrim heap1 primitives
allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, prim)
 = (heap', (name, addr))
   where
   (heap', addr) = hAlloc heap (NPrim name prim)
data Node = NAp Addr Addr                     -- Application
          | NSupercomb Name [Name] CoreExpr   -- Supercombinator
          | NNum Int                          -- Number
          | NInd Addr                         -- Indirection
          | NPrim Name Primitive              -- Primitive
          | NData Int [Addr]                  -- Tag, list of components
primDyadic :: TiState -> (Node -> Node -> Node) -> TiState
instantiateLet isrec defs body heap old_env
 = instantiate body heap1 new_env
   where 
   (heap1, extra_bindings) = mapAccuml instantiate_rhs heap defs
   new_env = extra_bindings ++ old_env
   rhs_env | isrec     = new_env
           | otherwise = old_env

   instantiate_rhs heap (name, rhs)
    = (heap1, (name, addr)) 
      where 
      (heap1, addr) = instantiate rhs heap rhs_env
scStep   :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body
 = (new_stack, dump, new_heap, globals, stats)
   where
   new_stack = drop (length arg_names) stack
   root = hd new_stack
   new_heap = instantiateAndUpdate body root heap (bindings ++ globals)
   bindings = zip2 arg_names (getargs heap stack)
instantiateAndUpdate (ENum n) upd_addr heap env 
 = hUpdate heap upd_addr (NNum n)
instantiateAndUpdate (EAp e1 e2) upd_addr heap env 
 = hUpdate heap2 upd_addr (NAp a1 a2) 
   where 
   (heap1, a1) = instantiate e1 heap  env
   (heap2, a2) = instantiate e2 heap1 env
instantiateAndUpdate (EVar v) upd_addr heap env
  = hUpdate heap upd_addr (NInd var_addr)
    where
    var_addr = aLookup env v
                     (error ("Undefined name " ++ show v))
instantiateAndUpdate (ELet isrec defs body) upd_addr heap old_env
 = instantiateAndUpdate body upd_addr heap1 new_env
   where 
   (heap1, extra_bindings) = mapAccuml instantiate_rhs heap defs
   new_env = extra_bindings ++ old_env
   rhs_env = if isrec then new_env else old_env

   instantiate_rhs heap (name, rhs)
    = (heap1, (name, addr)) 
      where 
      (heap1, addr) = instantiate rhs heap rhs_env
instantiateAndUpdate (EConstr tag arity) upd_addr h b 
            = instantiateAndUpdateConstr tag arity upd_addr h b
indStep :: TiState -> Addr -> TiState
indStep (a : stack, dump, heap, globals, stats) a'
 = (a' : stack, dump, heap, globals, stats)
tiFinal ([sole_addr], [], heap, globals, stats)
 = isDataNode (hLookup heap sole_addr)
tiFinal ([], dump, heap, globals, stats) = error "Empty stack!"
tiFinal state = False
apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2
 = ap_dispatch (hLookup heap a2)
   where
   ap_dispatch (NInd a3) = (stack, dump, heap', globals, stats)
                           where heap' = hUpdate heap ap_node (NAp a1 a3)
                                 ap_node = hd stack
   ap_dispatch node = (a1 : stack, dump, heap, globals, stats)
numStep (stack, stack':dump, heap, globals, stats) n
 = (stack', dump, heap, globals, stats)
primNeg :: TiState -> TiState
primNeg (stack, dump, heap, globals, stats)
 | length args /= 1 = error "primNeg: wrong number of args"
 | not (isDataNode arg_node) = ([arg_addr], new_stack:dump, heap, globals, stats)
 | otherwise = (new_stack, dump, new_heap, globals, stats)
   where
   args = getargs heap stack              -- Should be just one arg
   [arg_addr] = args
   arg_node = hLookup heap arg_addr        -- Get the arg node itself
   NNum arg_value = arg_node              -- Extract the value
   new_stack = drop 1 stack                -- Leaves root of redex on top
   root_of_redex = hd new_stack
   new_heap = hUpdate heap root_of_redex (NNum (-arg_value))
showState (stack, dump, heap, globals, stats)
 = iConcat [ showStack heap stack, iNewline, showDump dump, iNewline ]
showDump dump = iConcat [ iStr "Dump depth ", iNum (length dump) ]
extraPreludeDefs 
 = [ 
     ("False",       [], EConstr 1 0),
     ("True",        [], EConstr 2 0),
     ("not",         ["x"],     EAp (EAp (EAp (EVar "if") (EVar "x"))
                                          (EVar "False"))
                                     (EVar "True")),
     ("and",         ["x","y"], EAp (EAp (EAp (EVar "if") (EVar "x"))
                                          (EVar "y"))
                                     (EVar "False")),
     ("or",          ["x","y"], EAp (EAp (EAp (EVar "if") (EVar "x"))
                                          (EVar "True"))
                                     (EVar "y")),
     ("xor",         ["x","y"], EAp (EAp (EAp (EVar "if") (EVar "x"))
                                          (EAp (EVar "not") (EVar "y")))
                                     (EVar "y")),
     ("MkPair",      [], EConstr 1 2),
     ("fst",         ["p"], EAp (EAp (EVar "casePair") (EVar "p"))
                                 (EVar "K")),
     ("snd",         ["p"], EAp (EAp (EVar "casePair") (EVar "p"))
                                 (EVar "K1")),
     ("LCons",       [], EConstr 2 2),
     ("Nil",         [], EConstr 1 0),
     ("head",        ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs"))
                                       (EVar "abort"))
                                  (EVar "K")),
     ("tail",        ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs"))
                                       (EVar "abort"))
                                  (EVar "K1"))
   ]
data Primitive = Negate 
               | Add | Subtract 
               | Multiply | Divide
               | Greater | GreaterEq 
               | Less | LessEq
               | Eq | NotEq
               | PrimConstr Int Int          -- Tag and arity
               | If
               | PrimCasePair
               | PrimCaseList
               | Abort
primitives :: ASSOC Name Primitive
primitives = [  ("negate", Negate), 
               ("+", Add),           ("-", Subtract), 
               ("*", Multiply),      ("/", Divide),
               (">", Greater),       (">=", GreaterEq),
               ("<", Less),          ("<=", LessEq),
               ("==", Eq),           ("~=", NotEq),
               ("if", If),           ("casePair", PrimCasePair),
               ("caseList", PrimCaseList),
               ("abort", Abort)
             ]
instantiateConstr tag arity heap env
 = hAlloc heap (NPrim "Cons" (PrimConstr tag arity))
instantiateAndUpdateConstr tag arity upd_addr heap env
 = hUpdate heap upd_addr (NPrim "Cons" (PrimConstr tag arity))
isDataNode :: Node -> Bool
isDataNode (NNum n)    = True
isDataNode (NData t c) = True
isDataNode node        = False
step state
 = dispatch (hLookup heap (hd stack))
   where
   (stack, dump, heap, globals, stats) = state
   dispatch (NNum n)                  = numStep  state n
   dispatch (NInd a)                  = indStep  state a
   dispatch (NAp a1 a2)               = apStep   state a1 a2
   dispatch (NSupercomb sc args body) = scStep   state sc args body
   dispatch (NPrim name prim)         = primStep state prim
   dispatch (NData tag compts)        = dataStep state tag compts
dataStep (stack, stack':dump, heap, globals, stats) tag compts
 = (stack', dump, heap, globals, stats)
primStep state Negate   = primNeg state
primStep state Add      = primArith state (+)
primStep state Subtract = primArith state (-)
primStep state Multiply = primArith state (*)
primStep state Divide   = primArith state (div)
primStep state Greater    = primComp state (>)
primStep state GreaterEq  = primComp state (>=)
primStep state Less       = primComp state (<)
primStep state LessEq     = primComp state (<=)
primStep state Eq         = primComp state (==)
primStep state NotEq      = primComp state (/=)
primStep state (PrimConstr tag arity) = primConstr state tag arity
primStep state If           = primIf state
primStep state PrimCasePair = primCasePair state
primStep state PrimCaseList = primCaseList state

primStep state Abort = error "Program abort!"
primArith state op = primDyadic state op' 
                     where op' (NNum n) (NNum m) = NNum (op n m)
primComp state op  = primDyadic state op' 
                    where op' (NNum n) (NNum m) | op n m = NData 2 []
                                                | otherwise = NData 1 []
primDyadic (stack, dump, heap, globals, stats) op
 | length args /= 2 = error "primArith: wrong number of args"
 | not (isDataNode arg1_node) = ([arg1_addr], new_stack:dump, heap, globals, stats)
 | not (isDataNode arg2_node) = ([arg2_addr], new_stack:dump, heap, globals, stats)
 | otherwise = (new_stack, dump, new_heap, globals, stats)
   where
   args = getargs heap stack               -- Should be just two args
   [arg1_addr,arg2_addr] = args
   arg1_node = hLookup heap arg1_addr
   arg2_node = hLookup heap arg2_addr
   new_stack = drop 2 stack
   root_of_redex = hd new_stack
   new_heap = hUpdate heap root_of_redex (op arg1_node arg2_node)
primIf (stack, dump, heap, globals, stats)
 | length args < 3 = error "primIf: wrong number of args"
 | not (isDataNode arg1_node) = ([arg1_addr], new_stack:dump, heap, globals, stats)
 | otherwise = (new_stack, dump, new_heap, globals, stats)
   where
   args = getargs heap stack
   (arg1_addr:arg2_addr:arg3_addr:rest_args) = args
   arg1_node = hLookup heap arg1_addr
   new_stack = drop 3 stack
   root_of_redex = hd new_stack
   NData tag [] = arg1_node
   result_addr | tag == 2  = arg2_addr
               | otherwise = arg3_addr
   new_heap = hUpdate heap root_of_redex (NInd result_addr)
primCasePair (stack, dump, heap, globals, stats)
 | length args < 2 = error "primCasePair: wrong number of args"
 | not (isDataNode arg1_node) = ([arg1_addr], new_stack:dump, heap, globals, stats)
 | otherwise = (new_stack, dump, new_heap, globals, stats)
   where
   args = getargs heap stack
   (arg1_addr:arg2_addr:rest_args) = args
   arg1_node = hLookup heap arg1_addr
   new_stack = drop 2 stack
   root_of_redex = hd new_stack
   NData tag [fst,snd] = arg1_node
   new_heap = hUpdate heap1 root_of_redex (NAp temp_addr snd)
              where (heap1, temp_addr) = hAlloc heap (NAp arg2_addr fst)
primCaseList (stack, dump, heap, globals, stats)
 | length args < 3 = error "primCaseList: wrong number of args"
 | not (isDataNode arg1_node) = ([arg1_addr], new_stack:dump, heap, globals, stats)
 | otherwise = (new_stack, dump, new_heap, globals, stats)
   where
   args = getargs heap stack
   (arg1_addr:arg2_addr:arg3_addr:rest_args) = args
   arg1_node = hLookup heap arg1_addr
   new_stack = drop 3 stack
   root_of_redex = hd new_stack
   NData tag compts = arg1_node
   [head,tail] = compts
   new_heap | tag == 1 = hUpdate heap root_of_redex (NInd arg2_addr)
            | otherwise = hUpdate heap1 root_of_redex (NAp temp_addr tail)
              where (heap1, temp_addr) = hAlloc heap (NAp arg3_addr head)
primConstr (stack, dump, heap, globals, stats) tag arity
 | length args < arity = error "primConstr: wrong number of args"
 | otherwise = (new_stack, dump, new_heap, globals, stats)
   where
   args = getargs heap stack
   new_stack = drop arity stack
   root_of_redex = hd new_stack
   new_heap = hUpdate heap root_of_redex (NData tag args)
showNode (NAp a1 a2) = iConcat [ iStr "NAp ", showAddr a1, 
                                 iStr " ",    showAddr a2
                        ]
showNode (NSupercomb name args body) = iStr ("NSupercomb " ++ name)
showNode (NNum n) = (iStr "NNum ") `iAppend` (iNum n)
showNode (NInd a) = (iStr "NInd ") `iAppend` (showAddr a)
showNode (NPrim name prim) = iStr ("NPrim " ++  name)
showNode (NData tag compts) 
     = iConcat [ iStr "NData ", iNum tag, iStr " [",
                 iInterleave (iStr ",") (map showAddr compts),
                 iStr "]"
       ]
