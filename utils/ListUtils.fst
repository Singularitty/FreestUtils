module ListUtils where

import List

-------------------------------------------------------------------
--                      Extra Utils
-------------------------------------------------------------------

-- Prints the string representation of a value without including a \n char at the end
smPrint : forall a:*T . a -> ()
smPrint x = putStr $ show @a x

printCharArray : [Int] -> ()
printCharArray [] = putStrLn "\0"
printCharArray (x::xs) =
    let c = chr x in
    putChar c;
    printCharArray xs

-- Converts Int between 0-9 to Char
intToChar : Int -> Char
intToChar n = chr ((ord '0') + n)


-- Writes Int v at position n for a given list of ints
writeAt : [Int] -> Int -> Int -> [Int]
writeAt [] _ _ = []
writeAt (x::xs) n v
    | n < 0                 = error @[Int] "*** writeAt: negative index"
    | n > (length (x::xs))  = error @[Int] "*** writeAt: index too large"
    | n == 0                = v :: xs
    | otherwise             = x :: writeAt xs (n-1) v

-- Creates a list with size n filled with v
-- Used by createList, to avoid passing [] as a parameter
__createList : Int -> Int -> [Int] -> [Int]
__createList n v []
    | n <= 0        = error @[Int] "*** createList: Size less or equal to 0"
    | otherwise     = __createList (n-1) v (singleton v)
__createList n v arr
    | n == 0        = arr
    | otherwise     = v :: __createList (n-1) v arr

-- Creates a list with size n filled with v
createList : Int -> Int -> [Int]
createList n v = __createList n v []

-------------------------------------------------------------------
--                      Polymorphic Lists
-------------------------------------------------------------------


type Value = forall a:*T . a
data PolyList = Nil | List Value PolyList


pHead : PolyList -> Value
pHead Nil = error @Value "*** ListUtils.pHead: empty list"
pHead (List v _) = v

pTail : PolyList -> PolyList
pTail Nil = error @PolyList "*** ListUtils.pTail: empty list"
pTail (List _ x) = x

pLast : PolyList -> Value
pLast Nil = error @Value "*** ListUtils.pLast: empty list"
pLast (List v Nil) = v
pLast (List _ x) = pLast x

pNull : PolyList -> Bool
pNull Nil = True
pNull x   = False

pSingleton : Value -> PolyList
pSingleton v = List v Nil

pLength : PolyList -> Int
pLength Nil = 0
pLength (List _ x) = 1 + pLength x

pElemAt : PolyList -> Int -> Value
pElemAt Nil n
    | n < 0     = error @Value "*** ListUtils.pElemAt: negative index"
    | otherwise = error @Value "*** ListUtils.pElemAt: index too large"
pElemAt (List v xs) n
    | n == 0    = v
    | otherwise = pElemAt xs (n-1)

pConcat : PolyList -> PolyList -> PolyList
pConcat Nil Nil = error @PolyList "*** ListUtils.pConcat: Can't concatenate two empty lists"
pConcat Nil y   = y
pConcat x   Nil = x
pConcat (List v x) y = pConcat x (List v y)

pSplitAt : PolyList -> Int -> (PolyList, PolyList)
pSplitAt Nil _ = (Nil, Nil)
pSplitAt (List x xs) n
    | n <= 0 = (Nil, (List x xs))
    | otherwise = let (ys, zs) = pSplitAt xs (n-1) in ((List x xs), zs)

-------------------------------------------------------------------
--                      Matrices
-------------------------------------------------------------------

type Matrix = ([Int], Int)
type Coordinates = (Int, Int)

-- Reads element at position x,y from a given matrix
mRead : Matrix -> Coordinates -> Int
mRead matrix coords =
    let (array, width) = matrix in
    let (collum, row) = coords in 
    elemAt array (width * row + collum)
    
-- Write to position x,y to a given matrix
mWrite : Matrix -> Coordinates -> Int -> Matrix
mWrite matrix coords value =
    let (array, width) = matrix in
    let (collum, row) = coords in
    let index = width * row + collum in 
    (writeAt array index value, width)

-- Creates a matrix of size (x,y) filled with 0
mInit : Int -> Int -> Matrix
mInit x y =
    (createList (x*y) 0, x)

mInitFill: Int -> Int -> Int -> Matrix
mInitFill x y v =
    (createList (x*y) v, x)


__splitAndPrint : [Int] -> Int -> ()
__splitAndPrint [] _ = putStrLn "\0"
__splitAndPrint arr n  =
    let (x,xs) = splitAt n arr in
    print @[Int] x ; 
    __splitAndPrint xs n

-- Prints the matrix
mPrint : Matrix -> ()
mPrint m = 
    let (array, width) = m in
    __splitAndPrint array width

__splitAndPrintChar : [Int] -> Int -> ()
__splitAndPrintChar [] _ = putStrLn "\0"
__splitAndPrintChar arr n =
    let (x,xs) = splitAt n arr in
    printCharArray x;
    __splitAndPrintChar xs n

-- Converts the Ints in the buffers to chars and prints them
bufferPrint : Matrix -> ()
bufferPrint m =
    let (array, width) = m in
    __splitAndPrintChar array width
