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
--                           Lists
-------------------------------------------------------------------

-- String Lists

data StringList = SNil | SList String StringList

sHead : StringList -> String
sHead SNil = error @String "*** ListUtils.pHead: empty list"
sHead (SList v _) = v

sTail : StringList -> StringList
sTail SNil = error @StringList "*** ListUtils.pTail: empty list"
sTail (SList _ x) = x

sLast : StringList -> String
sLast SNil = error @String "*** ListUtils.pLast: empty list"
sLast (SList v SNil) = v
sLast (SList _ x) = sLast x

sNull : StringList -> Bool
sNull SNil = True
sNull x   = False

sSingleton : String -> StringList
sSingleton v = SList v SNil

sLength : StringList -> Int
sLength SNil = 0
sLength (SList _ x) = 1 + sLength x

sElemAt : StringList -> Int -> String
sElemAt SNil n
    | n < 0     = error @String "*** ListUtils.pElemAt: negative index"
    | otherwise = error @String "*** ListUtils.pElemAt: index too large"
sElemAt (SList v xs) n
    | n == 0    = v
    | otherwise = sElemAt xs (n-1)

sConcat : StringList -> StringList -> StringList
sConcat SNil SNil = error @StringList "*** ListUtils.pConcat: Can't concatenate two empty lists"
sConcat SNil y   = y
sConcat x   SNil = x
sConcat (SList v x) y = sConcat x (SList v y)

sSplitAt : StringList -> Int -> (StringList, StringList)
sSplitAt SNil _ = (SNil, SNil)
sSplitAt (SList x xs) n
    | n <= 0 = (SNil, (SList x xs))
    | otherwise = let (ys, zs) = sSplitAt xs (n-1) in ((SList x xs), zs)

__StringRepresentation : StringList -> Int -> ()
__StringRepresentation SNil _ = putStr "\0"
__StringRepresentation (SList s SNil) _ = putStr ", " ; putStr s ; putStrLn "]"
__StringRepresentation (SList s x) i
    | i == 0 = putStr "[" ; putStr s ; __StringRepresentation x (i+1)
    | otherwise = putStr ", " ; putStr s ;  __StringRepresentation x (i+1)

sPrint : StringList -> ()
sPrint SNil = putStr "[]\0"
sPrint x = __StringRepresentation x 0

-- Float Lists

data FloatList = FNil | FList Float FloatList

fHead : FloatList -> Float
fHead FNil = error @Float "*** ListUtils.fHead: empty list"
fHead (FList v _) = v

fTail : FloatList -> FloatList
fTail FNil = error @FloatList "*** ListUtils.fTail: empty list"
fTail (FList _ x) = x

fLast : FloatList -> Float
fLast FNil = error @Float "*** ListUtils.fLast: empty list"
fLast (FList v FNil) = v
fLast (FList _ x) = fLast x

fNull : FloatList -> Bool
fNull FNil = True
fNull _   = False

fSingleton : Float -> FloatList
fSingleton v = FList v FNil

fLength : FloatList -> Int
fLength FNil = 0
fLength (FList _ x) = 1 + fLength x

fElemAt : FloatList -> Int -> Float
fElemAt FNil n
    | n < 0     = error @Float "*** ListUtils.fElemAt: negative index"
    | otherwise = error @Float "*** ListUtils.fElemAt: index too large"
fElemAt (FList v xs) n
    | n == 0    = v
    | otherwise = fElemAt xs (n-1)

fConcat : FloatList -> FloatList -> FloatList
fConcat FNil y = y
fConcat x FNil = x
fConcat (FList v x) y = fConcat x (FList v y)

fSplitAt : FloatList -> Int -> (FloatList, FloatList)
fSplitAt FNil _ = (FNil, FNil)
fSplitAt (FList x xs) n
    | n <= 0 = (FNil, FList x xs)
    | otherwise = let (ys, zs) = fSplitAt xs (n-1) in (FList x ys, zs)

__FloatListRepresentation : FloatList -> Int -> ()
__FloatListRepresentation FNil _ = putStr "\0"
__FloatListRepresentation (FList s FNil) _ = putStr ", " ; smPrint @Float s ; putStrLn "]"
__FloatListRepresentation (FList s x) i
    | i == 0 = putStr "[" ; smPrint @Float s ; __FloatListRepresentation x (i+1)
    | otherwise = putStr ", " ; smPrint @Float s ;  __FloatListRepresentation x (i+1)

fPrint : FloatList -> ()
fPrint FNil = putStr "[]\0"
fPrint x = __FloatListRepresentation x 0

-- Char List

data CharList = CNil | CList Char CharList

cHead : CharList -> Char
cHead CNil = error @Char "*** ListUtils.cHead: empty list"
cHead (CList v _) = v

cTail : CharList -> CharList
cTail CNil = error @CharList "*** ListUtils.cTail: empty list"
cTail (CList _ x) = x

cLast : CharList -> Char
cLast CNil = error @Char "*** ListUtils.cLast: empty list"
cLast (CList v CNil) = v
cLast (CList _ x) = cLast x

cNull : CharList -> Bool
cNull CNil = True
cNull _   = False

cSingleton : Char -> CharList
cSingleton v = CList v CNil

cLength : CharList -> Int
cLength CNil = 0
cLength (CList _ x) = 1 + cLength x

cElemAt : CharList -> Int -> Char
cElemAt CNil n
    | n < 0     = error @Char "*** ListUtils.cElemAt: negative index"
    | otherwise = error @Char "*** ListUtils.cElemAt: index too large"
cElemAt (CList v xs) n
    | n == 0    = v
    | otherwise = cElemAt xs (n-1)

cConcat : CharList -> CharList -> CharList
cConcat CNil y = y
cConcat x CNil = x
cConcat (CList v x) y = cConcat x (CList v y)

cSplitAt : CharList -> Int -> (CharList, CharList)
cSplitAt CNil _ = (CNil, CNil)
cSplitAt (CList x xs) n
    | n <= 0 = (CNil, CList x xs)
    | otherwise = let (ys, zs) = cSplitAt xs (n-1) in (CList x ys, zs)

__CharListReepresentation : CharList -> Int -> ()
__CharListReepresentation CNil _ = putStr "\0"
__CharListReepresentation (CList s CNil) _ = putStr ", " ; putChar s ; putStrLn "]"
__CharListReepresentation (CList s x) i
    | i == 0 = putStr "[" ; putChar s ; __CharListReepresentation x (i+1)
    | otherwise = putStr ", " ; putChar s ;  __CharListReepresentation x (i+1)

cPrint : CharList -> ()
cPrint CNil = putStr "[]\0"
cPrint x = __CharListReepresentation x 0

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


