module ShowHeap where

import Template3 hiding (showResults, showState, runProg)
import Utils
import Language

showState (stack, dump, heap, globals, stats)
  = iConcat [ showHeap heap, iNewline ]

showHeap heap
  = iConcat [
        iStr "Heap {",
        iIndent (iInterleave iNewline (map show_heap_item (hAddresses heap))),
        iStr " }"
    ]
    where
      show_heap_item addr
        = iConcat [ showFWAddr addr, iStr ": ",
                    showStkNode heap (hLookup heap addr)
                  ]

showResults states
 = iDisplay (iConcat [ iLayn (map showState states),
                     showStats (last states)
          ])

runProg = showResults . eval . compile . parse

main = putStrLn $ runProg "id x = x; main = id (id 1)"
