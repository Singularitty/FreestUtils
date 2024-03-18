# FreestUtils

Was a bit bored and decided to try to program in FreeST https://freest-lang.github.io/


# Toys

## Snake

Functional snake game with score.

Use wasd to move the snake, requires inputing a move character and pressing enter to confirm, since there is no way to concurrently accept input without having the game loop thread waiting for it.

Food position is dependent on an initial seed, you can you use the default value of 42 or any other one you desire.

![Snake](images/snake.gif)

## Donut

Tried to implement a spinning donut using the algorithm in https://www.a1k0n.net/2011/07/20/donut-math.html, but the code runs to slow due to the way matrices are implemented.

# Utils

Implemented some usefull functionality in utils.fst:
- Matrices (String and Float matrices also, do not recommend using large matrices for these)
- String, Char and Float Lists, alongside some extra functionality
- Interesting Printing Funcitonalities
- Random Number Generator

## Pseudo-Random Number Generator

Generate a pseudo-random number for a given intial seed.

```Haskell
-- Seed for the PRGN
seed : Int
seed = 42

main : Int
main = prngGen seed
```

This uses the Linear Congruential generator algorithm

$X_{n+1} = (aX_n + c) mod m$

With the following parameters:

$a=74$

$c = 75$

$m = 65537$

## Matrices

Matrices are defined as an 1d list with x*y elements, with x being the width of the matrix.

Matrices can be created using:

```Haskell
-- Zero filled matrix
mInit x y
```

```Haskell
-- Value filled matrix
mInitFill x y value
```

These functions return a Matrix type, with x*y elements.

Aditionally, we can read and write to a matrix using:

```Haskell
-- Returns the Integer at position (x, y) in the matrix
mRead matrix (x, y)
```
```Haskell
-- Writes valuet to position (x, y) in the matrix
mWrite matrix (x, y) value
```

Finally we can print the matrix

```Haskell
-- Prints all the integers in the matrix
mPrint matrix
```

E.g:
```Haskell
main : String
main = 
    mPrint (mInitFill 10 10 36) ; 
    "\0" -- avoid printing () to the terminal
```

Output:

![Matrix Print](images/matrix_print.png)

One can also print the entire matrix as if it was a buffer of chars

```Haskell
main : String
main = 
    bufferPrint (mInitFill 10 10 36) ; 
    "\0" -- avoid priting () to the terminal
```

Output:

![Matrix Print](images/matrix_print_char.png)


