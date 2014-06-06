module Utils where
-- The following definitions are used to make some synonyms for routines
-- in the Gofer prelude to be more Miranda compatible
shownum n = show n
hd :: [a] -> a
hd  = head                        -- in Gofer standard prelude
tl :: [a] -> [a]
tl  = tail                        -- in Gofer standard prelude
zip2 :: [a] -> [b] -> [(a,b)]
zip2  = zip                       -- in Gofer standard prelude
-- can't do anything about # = length, since # not binary.
hInitial :: Heap a
hAlloc   :: Heap a -> a -> (Heap a, Addr)
hUpdate  :: Heap a -> Addr -> a -> Heap a
hFree    :: Heap a -> Addr -> Heap a
hLookup  :: Heap a -> Addr -> a
hAddresses :: Heap a -> [Addr]
hSize    :: Heap a -> Int
hNull      :: Addr
hIsnull    :: Addr -> Bool
showaddr :: Addr -> [Char]
type Heap a = (Int, [Int], [(Int, a)])
type Addr   = Int
hInitial                             = (0,      [1..],  [])
hAlloc  (size, (next:free), cts) n   = ((size+1, free,   (next,n) : cts),next)
hUpdate (size, free,        cts) a n = (size,   free,   (a,n) : remove cts a)
hFree   (size, free,        cts) a   = (size-1, a:free, remove cts a)
hLookup (size,free,cts) a
 = aLookup cts a (error ("can't find node " ++ showaddr a ++ " in heap"))
hAddresses (size, free, cts) = [addr | (addr, node) <- cts]
hSize (size, free, cts) = size
hNull    = 0
hIsnull a = a == 0
showaddr a = "#" ++ shownum a         -- Print # to identify addresses
remove :: [(Int,a)] -> Int -> [(Int,a)]
remove [] a = error ("Attempt to update or free nonexistent address #" ++
                     shownum a)
remove ((a',n):cts) a | a == a' = cts
                      | a /= a' = (a',n) : remove cts a
type ASSOC a b = [(a,b)]
aLookup []         k' def           = def
aLookup ((k,v):bs) k' def | k == k' = v
                          | k /= k' = aLookup bs k' def
aDomain :: ASSOC a b -> [a]
aDomain alist = [key | (key,val) <- alist]
aRange :: ASSOC a b -> [b]
aRange  alist = [val | (key,val) <- alist]
aEmpty = []
getName  :: NameSupply -> [Char]   -> (NameSupply, [Char])
getNames :: NameSupply -> [[Char]] -> (NameSupply, [[Char]])
initialNameSupply :: NameSupply
type NameSupply = Int
initialNameSupply = 0
getName name_supply prefix = (name_supply+1, makeName prefix name_supply)
getNames name_supply prefixes
 = (name_supply + length prefixes, zipWith makeName prefixes [name_supply..])
makeName prefix ns = prefix ++ "_" ++ shownum ns
setFromList     :: (Ord a) => [a]     -> Set a
setToList       :: (Ord a) => Set a   -> [a]
setUnion        :: (Ord a) => Set a   -> Set a -> Set a
setIntersection :: (Ord a) => Set a   -> Set a -> Set a
setSubtraction  :: (Ord a) => Set a   -> Set a -> Set a
setElementOf    :: (Ord a) => a       -> Set a -> Bool
setEmpty        :: (Ord a) => Set a
setIsEmpty      :: (Ord a) => Set a   -> Bool
setSingleton    :: (Ord a) => a       -> Set a
setUnionList    :: (Ord a) => [Set a] -> Set a
type Set a = [a]           -- Ordered by the sort function
setEmpty = []
setIsEmpty s = null s
setSingleton x = [x]
setFromList = rmdup . sort
                  where rmdup []       = []
                        rmdup [x]      = [x]
                        rmdup (x:y:xs) | x == y = rmdup (y:xs)
                                       | x /= y = x: rmdup (y:xs)
setToList xs = xs
setUnion []     []            = []
setUnion []     (b:bs)        = (b:bs)
setUnion (a:as) []            = (a:as)
setUnion (a:as) (b:bs) | a <  b  = a: setUnion as (b:bs)
                       | a == b  = a: setUnion as bs
                       | a >  b  = b: setUnion (a:as) bs
setIntersection []     []     = []
setIntersection []     (b:bs) = []
setIntersection (a:as) []     = []
setIntersection (a:as) (b:bs) | a <  b = setIntersection as (b:bs)
                              | a == b = a: setIntersection as bs
                              | a >  b = setIntersection (a:as) bs
setSubtraction []     []      = []
setSubtraction []     (b:bs)  = []
setSubtraction (a:as) []      = (a:as)
setSubtraction (a:as) (b:bs) | a <  b  = a: setSubtraction as (b:bs)
                             | a == b  = setSubtraction as bs
                             | a > b   = setSubtraction (a:as) bs
setElementOf x []            = False
setElementOf x (y:ys)        = x==y || (x>y && setElementOf x ys)
setUnionList = foldll setUnion setEmpty
first (a,b) = a
second (a,b) = b
-- zipWith is defined in standard prelude
foldll :: (a -> b -> a) -> a -> [b] -> a
foldll = foldl                       -- in Gofer standard prelude.
mapAccuml :: (a -> b -> (a, c)) -- Function of accumulator and element
                                --   input list, returning new
                                --   accumulator and element of result list
             -> a               -- Initial accumulator
             -> [b]             -- Input list
             -> (a, [c])        -- Final accumulator and result list
mapAccuml f acc []     = (acc, [])
mapAccuml f acc (x:xs) = (acc2, x':xs')
                         where (acc1, x')  = f acc x
                               (acc2, xs') = mapAccuml f acc1 xs
sort [] = []
sort [x] = [x]
sort (x:xs) = [ y | y <- xs, y < x] ++ x : [ y | y <- xs, y >= x ]
space n = take n (repeat ' ')
