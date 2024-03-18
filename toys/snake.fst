import ListUtils
import Random

-- Point List Definition

type Point = (Int, Int)
data PointList = PNil | PList Point PointList

pHead : PointList -> Point
pHead PNil = error @Point "*** snake.pHead: empty List"
pHead (PList p _) = p

pTail : PointList -> PointList
pTail PNil = error @PointList "*** snake.pTail: empty List"
pTail (PList _ ps) = ps

pLenght : PointList -> Int
pLenght PNil = 0
pLenght (PList _ ps) = 1 + pLenght ps

pElemAt : PointList -> Int -> Point
pElemAt PNil n
    | n < 0 = error @Point "*** snake.pElemAt: negative index"
    | otherwise = error @Point "*** snake.pElemAt: index to large"
pElemAt (PList p ps) n
    | n == 0 = p
    | otherwise = pElemAt ps (n-1)

pSingleton : Point -> PointList
pSingleton p = PList  p PNil

pWriteAt : PointList -> Int -> Point -> PointList
pWriteAt PNil _ _ = PNil
pWriteAt (PList p ps) n point
    | n < 0     = error @PointList "*** snake.pWriteAt: negative index"
    | n > pLenght (PList p ps) = error @PointList "*** snake.pWriteAt: index too large"
    | n == 0 = PList point ps
    | otherwise = PList p (pWriteAt ps (n-1) point)

pSplitAt : PointList -> Int -> (PointList, PointList)
pSplitAt PNil _ = (PNil, PNil)
pSplitAt (PList x xs) n
    | n <= 0 = (PNil, (PList x xs))
    | otherwise = let (ys, zs) = pSplitAt xs (n-1) in ((PList x ys), zs)

pConcat : PointList -> PointList -> PointList
pConcat PNil PNil = error @PointList "*** snake.pConcat: Can't concatenate two empty lists"
pConcat PNil y   = y
pConcat x   PNil = x
pConcat (PList v x) y = pConcat x (PList v y)

pInList : PointList -> Point -> Bool
pInList PNil _ = False
pInList (PList p ps) point =
    let (x, y) = p in
    let (x', y') = point in
    if (x==x' && y==y') then True
    else pInList ps point


-- Input

type InputService : 1A = +{GetDirection: ?Int} ; Close

inputClient : InputService -> Int
inputClient c =
    c |> select GetDirection |> receiveAndClose @Int

inputServer : dualof InputService -> ()
inputServer (GetDirection c) =
    c |> send getDirection |> wait

__determineDirection : Char -> Int
__determineDirection c
    | charEq 'w' c = 0
    | charEq 'd' c = 1
    | charEq 's' c = 2
    | charEq 'a' c = 3
    | otherwise = print @Char c ; error @Int "*** snake.__deterineDirection: Invalid input direction"

getDirection : Int
getDirection = putStrLn "Next move? (w - up, s - down, d - right, a - left)" ; __determineDirection (readChar getLine)

getUserSeed : Int
getUserSeed = 
    sMatrixPrint clearScreen ;
    putStr "\x1b[H" ;
    putStrLn "Custom seed? (default 42)";
    let seed = readInt getLine in
    if (seed < 0) then
        initialSeed
    else
        seed

-- Game Variables

type Snake = PointList
type Food = Point
type World = StringMatrix
type Score = Int
type Game = (Snake, Food)

box_x : Int 
box_y : Int

box_x = 20
box_y = 10

initialSeed : Int
initialSeed = 42

snake : Snake
snake = PList (box_x / 2 - 1,box_y / 2 - 1) PNil

-- Initialize Game World

border : String
border = "░"

foodChar : String
foodChar = "¤"

snakeChar : String
snakeChar = "■"

topBorder : StringList
topBorder = sCreateList box_x border

midBorder : StringList
midBorder = sWriteAt (sWriteAt  (sCreateList box_x " ") 0 border) (box_x-1) border

worldGenerator : StringList -> Int -> StringList
worldGenerator s n
    | n == 0 = sConcat s topBorder
    | otherwise = worldGenerator (sConcat midBorder s) (n-1)

world : World
world = (worldGenerator topBorder (box_y-2), box_x)

-- Drawing Functions

drawStrings : PointList -> StringMatrix -> String -> StringMatrix
drawStrings PNil w _ = w
drawStrings (PList p PNil) w s =
    let (x,y) = p in
    msWrite w (x,y) s
drawStrings (PList p ps) w s =
    let (x,y) = p in
    drawStrings ps (msWrite w (x,y) s) s

drawSnake : Snake -> World -> World
drawSnake snake' world' = drawStrings snake' world' snakeChar

drawFood : Food -> World -> World
drawFood food' world' = drawStrings (pSingleton food') world' foodChar

renderFrame : Snake -> Food -> Score -> ()
renderFrame s f score =
    sMatrixPrint (drawSnake s (drawFood f world)) ; putStr "Score: " ; print @Int score

clearScreen : StringMatrix
clearScreen = msInitFill 1 50 " "

gameOver : Score -> ()
gameOver score' = sMatrixPrint clearScreen ; putStr "\x1b[H" ; putStr "\n\n\n\n         Game Over!\n          Score:" ; smPrint @Int score'; putStrLn "\n    Thank you for playing!\n\n\n\n\n\n"


-- Game Logic

__newHead : Int -> Point -> Point
__newHead dir head
    | dir == 0 = let (x,y) = head in (x,y-1)
    | dir == 1 = let (x,y) = head in (x+1,y)
    | dir == 2 = let (x,y) = head in (x,y+1)
    | otherwise = let (x,y) = head in (x-1,y)

generateNewFood : Int -> (Point, Int)
generateNewFood seed =
    let rng1 = prngGen seed in
    let rng2 = prngGen rng1 in
    let x = min (box_x-2) (max (mod rng1 box_x) 1) in
    let y = min (box_y-2) (max (mod rng2 box_y) 1) in 
    ((x, y), rng2)


gameLoop : Game -> Int -> ()
gameLoop game seed =
    let (snake', food') = game in
    let (xFood, yFood) = food' in
    let head = pHead snake' in
    let c = forkWith @InputService @() inputServer in
    let direction = inputClient c in
    let newHead = __newHead direction head in
    let (xHead, yHead) = newHead in
    -- Check for colisions with the borders
    -- putStr "xHead: " ; smPrint @Int xHead ; putStr " xFood: " ; smPrint @Int xFood ;
    if (xHead == 0 || yHead == 0 || xHead == (box_x-1) || yHead == (box_y-1)) then
        gameOver ((pLenght snake')-1)
    -- Check for colisions with the snake body
    else if (pInList (pTail snake') (xHead, yHead)) then
        gameOver ((pLenght snake')-1)
    -- Check for colisions with food 
    else if (xHead == xFood && yHead == yFood) then
        let newSnake = pConcat (pSingleton newHead) snake' in
        let (newFood, nextSeed) = generateNewFood seed in
        putStr "\x1b[H" ;
        renderFrame newSnake (food') ((pLenght newSnake)-1) ;
        gameLoop (newSnake, newFood) nextSeed
    else
        let (nextBody, _) = pSplitAt snake' ((pLenght snake')-1) in
        let newSnake = pConcat (pSingleton newHead) nextBody in
        putStr "\x1b[H" ;
        renderFrame newSnake food' ((pLenght newSnake)-1) ;
        gameLoop (newSnake, food') seed

main : String
main = 
    let (food, seed) = generateNewFood getUserSeed in
    sMatrixPrint clearScreen ; putStr "\x1b[H" ; renderFrame snake food 0 ; gameLoop (snake, food) seed; "\0"
