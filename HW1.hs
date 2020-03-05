module HW1 (
    form,
    constGrid,
    flatten,
    access,
    slice,
    vcat,
    hcat,
    without,
    matches2d
) where

-- do not modify the module declaration above!
-- this will ensure that you cannot load (compile)
-- the module without implementing all of the functions.

-- If you have functions you do not want to implement,
-- leave them as undefined or make them have another
-- default value. If you fully remove any of their definitions,
-- that will be a compilation error during evaluation,
-- and you will be eligible for (yay!) a 5 point deduction
-- (that's bad for your grade). Runtime errors in your code 
-- (rather than compilation errors) are acceptable and will simply
-- result in you getting zero from the specific test case causing
-- an error.

-------------------------
--
-- Fellowship of the Grid (25, 5, 5, 5 points)
form :: [a] -> (Int, Int) -> [[a]] 
form [] (_,_) = []
form lst (a,b) = take b lst : [] ++ form(drop b lst ) (a,b)


constGrid :: a -> (Int, Int) -> [[a]]
constGrid _ (_,0) = []
constGrid value (_,1) = [value] : [] 
constGrid value (a,b) = [value] : constGrid value (a,b-1)

flatten :: [[a]] -> [a]
flatten _ = undefined 

access :: [[a]] -> (Int, Int) -> a
access lst (a,b) = (lst !! a )!! b
----------------------------
-- The Two Signatures (10, 5, 5, 10 points)
slice :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
slice _ _ _ = undefined 

vcat :: [[a]] -> [[a]] -> [[a]]
vcat _ _ = undefined

hcat :: [[a]] -> [[a]] -> [[a]]
hcat _ _ = undefined

without :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
without _ _ _ = undefined
----------------------------
-- Return of the Non-trivial (30 points, 15 subject to runtime constraints)
matches2d :: Eq a => [[a]] -> [[a]] -> [(Int, Int)]
matches2d _ _ = undefined
----------------------------
-- What is undefined? Just a value that will cause an error
-- when evaluated, from the GHC implementation:
-- undefined = error "Prelude.undefined"
-- But it allows your module to be compiled
-- since the function definitions will exist.
