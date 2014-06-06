{-----------------------------------------------------------------------

	Standalone G-Machine Code Generator
	(c) Kevin Hammond, St Andrews University, 1997

	This program generates MIPS machine code from the G-Machine
	intermediate form defined in the textbook by Peyton Jones
	and Lester.  It was written as part of a second course
	on compilers, focussing on machine-dependent translation.

	The resulting code has been tested for simple
	cases using the SPIM simulator.  It is very instructive
	to see the in-memory heap layout etc. and step through the
	instruction sequences.

	The program should be compatible with HUGS, GHC and HBC
	for Haskell 1.3.  Newer versions have not been tested. 

-----------------------------------------------------------------------}

import Utils			-- Heap etc. definitions
import Language			-- Parser
import GM hiding (pop)		-- G-machine definitions and compiler

-- The machine program

main = interact compileProg
--{-
compileProg = machinecode . compile . parse
---}
{-
compileProg = machinecode . compileInits . compile . parse
	where compileInits s = putGlobals (g0++g1) (putHeap h1 s)
		where h0 = getHeap s
		      g0 = getGlobals s
		      (h1,g1) = mapAccuml allocateSc h0 compiledPrimitives
-}

stackmachine = False		-- True if ops work on stack/heap
				-- rather than V stack

-- Generate the machine code

machinecode s = 
	showi [gcode,gdata,gruntime] ++ "\n"

	where h = getHeap s
	      g = getGlobals s

	      -- The startup code and translation of standard primitives
	      gcode = showi [startup_code,
	   	             label "main",
	                     cg g (getCode s),
	                     halt,
	                     translateHeapCode g h
			    ]

	      -- The Data area
	      gdata = showi ["\n.data",
	        	     translateHeap h,

	        	     label "heap",
			     ".space 1024",

			     label "stack",
		             ".space 512",


			     label "vstack",
		             ".space 64"
	       		    ]

  	      -- The runtime system code: eval, print and unwind
	      gruntime = ".text\n" ++ eval_code ++ print_code ++ unwind_code


{-
	Eval takes the node on the top of the stack, and reduces it to
	normal form.

--		Stack			Dump
--
--		a : s			d
--	==>	a : ret : d : s	
	
-}

eval_code =
    showi [
	".text",
	label "eval",
	lw node_reg sp,				-- Node   <- [Sp]
	putsp dump_reg,				-- [Sp]   <- Dump
	pushsp ret_reg,				-- [++Sp] <- Ret
	move dump_reg sp,			-- Dump   <- Sp
	pushsp node_reg,	       		-- [++Sp] <- Node
	b "unwind"				-- goto Unwind
    	]

{-
	Print inspects the node on the top of the stack,
	and prints it if it's an integer.
-}

print_code = 
    showi [
	label "print",
	lw node_reg sp,				-- Node <- [Sp]
	lw tag_reg node_reg,			-- Tag  <- GET_TAG(Node)
	li temp_reg int_tag,
	beq tag_reg temp_reg "print_int",	-- if Tag == INT goto print_int
	return,

	label "print_int",
	lwi sysarg_reg 1 node_reg,     		-- SysArg <- [Node+1]
	print_int,				-- call print_int
	pop 1,					-- Pop the node
	return
    	]

{-
	Unwind unwinds the TOS until it finds a supercombinator.
	It then evaluates the supercombinator.
-}

unwind_code = start_unwind_code ++ unwind_ap_code ++ unwind_ind_code ++ 
	      unwind_int_code ++ unwind_global_code

-- Choose the appropriate unwind sequence

start_unwind_code =
    showi [
	label "unwind",
	lw node_reg sp,				-- Node <- [Sp]
	lw tag_reg node_reg,			-- Tag  <- GET_TAG(Node)
	li temp_reg ap_tag,
	beq tag_reg temp_reg "unwind_ap", 	-- if Tag == AP goto unwind_ap
	li temp_reg ind_tag,
	beq tag_reg temp_reg "unwind_ind", 	-- if Tag == IND goto unwind_ind
	li temp_reg global_tag,
	beq tag_reg temp_reg "unwind_global", 	-- if Tag == GLOBAL goto unwind_global
	li temp_reg int_tag,
	beq tag_reg temp_reg "unwind_int", 	-- if Tag == INT goto unwind_int
	halt					-- Error -- Unknown tag!
	]

-- Unwinding applications, push the function part on the stack

--		Stack		Heap
--
--		a : s		a: AP f x
--	==>	f : a : s

unwind_ap_code =
    showi [
	label "unwind_ap",
		lwi temp2_reg 1 node_reg,	-- [++Sp] <- GET_AP_FN(Node)
		pushsp temp2_reg,
		b "unwind"
    	]

-- Unwinding indirections, grab the indirection onto the stack.

--		Stack		Heap
--
--		a : s		a: IND a'
--	==>	a' : s

unwind_ind_code =
    showi [
	label "unwind_ind",
		lwi temp2_reg 1 node_reg,	-- Temp2 <- GET_IND(Node)
		putsp temp2_reg,		-- [Sp]  <- Temp2
		b "unwind"			-- goto unwind
	]

--		Stack				Heap		Dump
--
--		a : ... : i : d' : s'		a: INT n	<i,d',s'>
--	==>	a : s'						d'

unwind_int_code =
    showi [
	label "unwind_int",
		lw temp_reg dump_reg,		-- Temp <- GET_RET_ADDR(Dump)
		getsp node_reg,			-- Node <- [Sp]
		move sp dump_reg,		-- Sp   <- Dump
		pop 1,				-- Clear old dump frame
		lwi dump_reg (-1) dump_reg,	-- Dump <- GET_OLD_DUMP(Dump)
		putsp node_reg,			-- [Sp] <- Node
		jr temp_reg
	]


--

--		Stack				Heap
--
--		f : a0 : ... : an : s		f : GLOBAL c n
--						a0 : AP f a0'
--						...
--						an : AP f an'
--
--	==>	a0' : ... : an' : an : s'				

unwind_global_code =
    showi [
	label "unwind_global",
		lwi ret_reg 1 node_reg,		-- Ret <- code for supercombinator
		lwi temp_reg 2 node_reg,	-- Temp <- no. of args. on stack
		slli temp_reg 2,		-- Convert to addresses

		-- get the argument from the root of the redex	
		sub2 temp3_reg sp temp_reg,	-- Temp3 <- address of root on stack
		lw node_reg temp3_reg,		-- Node  <- apply node
		lwi arg_reg 2 node_reg,		-- Arg   <- argument from apply node
	
		-- Now unwind Temp apply nodes onto the stack
		label "unwind_loop",
			beq temp3_reg sp "unwind_done",

			addi temp3_reg 4,	-- Temp3 <- stack loc. of next apply node
			lw node_reg temp3_reg,	-- Node  <- next apply node
			sw arg_reg temp3_reg,	-- replace stack val. by Arg 
			lwi arg_reg 2 node_reg,	-- Arg   <- GET_AP_ARG(Node)
			b "unwind_loop",

		label "unwind_done",
		jr ret_reg
	]

--	Initialise Sp, Hp and Dump
--	Sp <- &stack, Hp <- &Heap, Dump <- Sp

startup_code =
	showi [la sp "stack",la hp "heap",la vsp "vstack",move dump_reg sp]

--	Generate code for a list of instructions in the context of globals g
--	

cg :: GmGlobals -> [Instruction] -> String
cg g is = concat (map (\ i -> showi [cgComment i, codeGen g i] ++ "\n\n") is)


--	Add a comment for each G-machine instruction

cgComment :: Instruction -> String

cgComment (Slide n)      =   "# Slide "       ++ show n
cgComment (Alloc n)      =   "# Alloc "       ++ show n
cgComment (Update n)     =   "# Update "      ++ show n
cgComment (Pop n)        =   "# Pop "         ++ show n
cgComment Unwind         =   "# Unwind"
cgComment (Pushglobal f) =   "# Pushglobal "  ++ f
cgComment (Pushint n)    =   "# Pushint "     ++ show n
cgComment (Push n)       =   "# Push "        ++ show n
cgComment Mkap           =   "# Mkap"
cgComment Eval           =   "# Eval"
cgComment Add            =   "# Add"
cgComment Sub            =   "# Sub"
cgComment Mul            =   "# Mul"
cgComment Div            =   "# Div"
cgComment Neg            =   "# Neg"
cgComment Eq             =   "# Eq"
cgComment Ne             =   "# Ne"
cgComment Le             =   "# Le"
cgComment Lt             =   "# Lt"
cgComment Ge             =   "# Ge"
cgComment Gt             =   "# Gt"
cgComment Print          =   "# Print"
cgComment Mkbool         =   "# Mkbool"
cgComment Mkint          =   "# Mkint"
cgComment Get            =   "# Get"
cgComment (Pushbasic n)  =   "# Pushbasic "     ++ show n
cgComment (Cond t f)     =   "# Cond "
cgComment _              =   "# Unknown G-machine instruction "

-- The code generator itself

codeGen :: GmGlobals -> Instruction -> String

codeGen g (Slide n)      =   gslide n
codeGen g (Alloc n)      =   galloc n
codeGen g (Update n)     =   gupdate n
codeGen g (Pop n)        =   gpop n
codeGen g Unwind         =   gunwind
codeGen g (Pushglobal f) =   gpushglobal ("h_" ++ show (aLookup g f (error ("Undeclared global " ++ f))))
codeGen g (Pushint n)    =   gpushint n
codeGen g (Push n)       =   gpush n
codeGen g Mkap           =   gmkap
codeGen g Eval           =   geval
codeGen g Add            =   gadd
codeGen g Sub            =   gsub
codeGen g Mul            =   gmul
codeGen g Div            =   gdiv
{-
codeGen g Eq             =   geq
codeGen g Ne             =   gne
codeGen g Gt             =   ggt
codeGen g Ge             =   gge
codeGen g Ge             =   gge
codeGen g Lt             =   glt
codeGen g Le             =   gle
-}
codeGen g Print		 =   gprint
codeGen g (Pushbasic n)  =   gpushbasic n
codeGen g Mkbool         =   gmkbool
codeGen g Mkint          =   gmkint
codeGen g Get		 =   gget
codeGen _ _		 =   "halt"

-- G-Machine Instructions: Stack
gslide n = 	showi [getsp node_reg, pop n, putsp node_reg]
galloc n = 	showi [showi [alloc_ind 0, pushhp] | i <- [1..n]]
gupdate n = 	showi [lwi node_reg (-(n+1)) sp,lw temp_reg sp,swi temp_reg 1 node_reg,
		       li tag_reg ind_tag, sw tag_reg node_reg,pop 1]
gpop =		pop
gpush n =	showi [lwi node_reg (-n) sp, pushsp node_reg]

-- G-Machine Instructions: Graph
gpushglobal l = showi [la temp_reg l, pushsp temp_reg]
gpushint n =	showi [pushhp, alloc_int n]

gmkap =		showi [
			popsp temp_reg,
			popsp temp2_reg,
			pushhp,
			alloc_ap temp_reg temp2_reg
		]

-- G-Machine Instructions: Evaluation
gunwind = 	b "unwind"
geval =		call "eval"
gprint =	call "print"

-- G-Machine Instructions: VStack
gpushbasic n =	showi [li temp_reg n, pushvsp temp_reg]
gget =		showi [popsp node_reg,
		       lwi temp_reg 1 node_reg,
		       pushvsp temp_reg
		      ]

gmkint =	showi [popvsp temp_reg,
			pushhp,				-- Create a new one in heap
			halloc int_tag,
			hallocr temp_reg
		      ]

gmkbool =	showi [popvsp temp_reg,
			pushhp,				-- Create a new one in heap
			halloc bool_tag,
			hallocr temp_reg
		      ]

-- G-Machine Primitives
gadd =		prim2 add
gsub =		prim2 sub
gmul =		prim2 mul
gdiv =		prim2 divide
{-
geq =		prim2 eq
gne =		prim2 ne
glt =		prim2 lt
gle =		prim2 le
ggt =		prim2 gt
gge =		prim2 ge
-}

-- Generic Primitive code for stack code
prim2 op =	if stackmachine then
		showi [
			popsp temp_reg,			-- Addresses of args
			popsp temp2_reg,
			lwi temp_reg 1 temp_reg,	-- Values of args
			lwi temp2_reg 1 temp2_reg,
			op temp_reg temp2_reg,		-- The actual op
			pushhp,				-- Create a new one in heap
			halloc int_tag,
			hallocr temp_reg
		]
		else
		showi [
			popvsp temp_reg,		-- Values of args
			popvsp temp2_reg,
			op temp_reg temp2_reg,		-- The actual op
			pushvsp temp_reg
		]



-- Stack Manipulation
popsp r =	showi [getsp r, decsp]
pushsp r =	showi [incsp, putsp r]
pushhp =	pushsp hp

getsp r =	lw r sp
putsp r =	sw r sp
incsp = 	addsp 1
decsp = 	addsp (-1)
addsp n = 	addi sp (n*4)
pop n = 	addsp (-n)

-- Heap Allocation
halloc n =	showi[li atemp_reg n, hallocr atemp_reg]
halloca l =	showi[la atemp_reg l, hallocr atemp_reg]
hallocr r = 	showi [sw r hp,addi hp 4]

-- VStack Manipulation
popvsp r =	showi [getvsp r, decvsp]	-- Pop the top of the V stack into a register
pushvsp r =	showi [incvsp, putvsp r]	-- Push a register onto the V stack

getvsp r =	lw r vsp
putvsp r =	sw r vsp
incvsp = 	addvsp 1
decvsp = 	addvsp (-1)
addvsp n = 	addi vsp (n*4)
popv n = 	addvsp (-n)

-- Node tags
int_tag = 	0
bool_tag = 	1
ind_tag = 	2
ap_tag = 	3
global_tag = 	4

-- Allocating nodes
alloc_int n =		showi [halloc int_tag, halloc n]
alloc_bool b =		showi [halloc bool_tag, halloc b]
alloc_ind i =	 	showi [halloc ind_tag, hallocr i]
alloc_ap f x =	 	showi [halloc ap_tag, hallocr f, hallocr x]
alloc_global g n = 	showi [halloc global_tag,halloc n,halloca g]


-- Real Machine Instructions

-- Arithmetic Instructions
add r1 r2 = 	"add "++reg r1++","++reg r2++","++reg r1	-- Add reg r2 to reg r1
sub r1 r2 = 	"sub "++reg r1++","++reg r2++","++reg r1	-- Subtract reg r2 from reg r1
mul r1 r2 = 	"mul "++reg r1++","++reg r2++","++reg r1	-- Multiply reg r2 by reg r1
divide r1 r2 = 	"div "++reg r1++","++reg r2++","++reg r1	-- Divide reg r2 by reg r1
sub2 r1 r2 r3 =	"sub "++reg r1++","++reg r2++","++reg r3	-- Subtract reg r3 from reg r2 giving reg r1
addi r n = 	"addi "++reg r++","++show n			-- Add immediate n to reg r
slli r n = 	"sll "++reg r++","++reg r++","++show n		-- Shift left logical reg r by n

-- Memory Instructions
sw s d =	"sw "++reg s++",("++reg d++")"			-- Store reg s to [reg d]
swi s o d =	"sw "++reg s++","++show (o*4)++"("++reg d++")"	-- Store reg s to [o+reg d]
lw d s =	"lw "++reg d++",("++reg s++")"			-- Load reg d with [reg s]
lwi d o s =	"lw "++reg d++","++show (o*4)++"("++reg s++")"	-- Load reg d with [o+reg s]

-- Immediate instructions
li r n =	"li "++reg r++","++show n			-- Load immediate n to reg r
la r a =	"la "++reg r++","++a				-- Load address labelled l in reg r
move r1 r2 =	"move "++reg r1++","++reg r2			-- Copy reg r1 to reg r2

-- Branch Instructions
beq r1 r2 l =	"beq "++reg r1++","++reg r2++","++l		-- Branch if r1=r2 to l
b l =		"b "++l						-- Branch to l
jr r =		"jr "++reg r					-- Branch to reg r
call l =	"jal "++l					-- Branch to l and save pc in $ra
halt =		"halt"						-- Stop!
return =	jr ret_reg					-- Jump to the saved pc in $ra

-- Assembler pseudo-ops
label l =	"\n" ++ l ++ ":"				-- Label l
word n =	".word " ++ (show n)				-- Word pseudo-op
wordl l =	".word " ++ l					-- wordl with a label

-- System Calls
print_int =	showi ["li $v0,1","syscall"]


-- Register allocations
hp = 		hp_reg
sp = 		sp_reg
vsp = 		vsp_reg
zero =		zero_reg

-- Global Register Assignments
hp_reg =	8		-- Heap Pointer
sp_reg = 	9		-- Stack Pointer
vsp_reg = 	10		-- VStack Pointer
dump_reg =	11		-- Dump

-- Convenient local uses
node_reg =	12		-- Node
tag_reg =	13		-- Tag
arg_reg =	14		-- Args

-- Temporaries
temp_reg =	15
temp2_reg =	16
temp3_reg =	17
atemp_reg =	18		-- Reserved for use during allocation

-- System Register Assignments
ret_reg =	31		-- Return Address, used by jal
sysarg_reg =	4		-- Argument to syscall
zero_reg =	0		-- Constant zero

-- Defining registers
reg r = "$"++show r		-- MIPS registers are prefixed by '$', e.g. '$8'


{-
	showi lays out instructions with a newline between each
	instruction.
-}

-- Showing instructions
showi [] =	""
showi [i] =	i
showi (i:is) =	i ++ "\n" ++ showi is


-- Translate Code for Supercombinators

translateHeapCode :: GmGlobals -> GmHeap -> String
translateHeapCode g (n,free,objs) = concat (map translateHeapCode' (take n objs))
  where
	translateHeapCode' (a,(NGlobal i c)) = showi [glabel a, cg g c]
	translateHeapCode' h = ""

-- Translate the heap referred to by the supercombinator
translateHeap :: GmHeap -> String
translateHeap (n,free,objs) = concat (map translateHeap' (take n objs))
   where 
	translateHeap' (a,n) = showi [hlabel a,translateHeap'' n]
	  where
		translateHeap'' (NNum n) = 	showi ["# Int"++show n,word int_tag, word n]
		translateHeap'' (NConstr b []) =showi ["# Bool"++show b,word bool_tag, word b]
		translateHeap'' (NInd a') = 	showi [word ind_tag, rhlabel a']
		translateHeap'' (NGlobal i c) = 
			showi ["# Global",word global_tag,wordl (rglabel a),word i]
		translateHeap'' (NAp f x) = 	showi [word ap_tag,word (rhlabel f),wordl (rhlabel x)]

rlabel c a = c: "_"++show a
rhlabel a = rlabel 'h' a
rglabel a = rlabel 'g' a
hlabel a = label (rhlabel a)
glabel a = label (rglabel a)
alt_label n = label ("l_"++show n)

