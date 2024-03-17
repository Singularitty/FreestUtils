module Random where

--seed : Int
--seed = 42

a : Int
c : Int
m : Int

-- Good values for a generator

a = 74
c = 75
m = 65537

prngGen : Int -> Int
prngGen seed = mod (a * seed + c) m