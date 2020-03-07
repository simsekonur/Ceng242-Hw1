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
import Data.List
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
constGrid value (a,b) = take a (repeat (take b (repeat value)))

flatten :: [[a]] -> [a]
flatten xss = [x| xs <- xss , x <- xs ] 

access :: [[a]] -> (Int, Int) -> a
access lst (a,b) = (lst !! a )!! b
----------------------------
-- The Two Signatures (10, 5, 5, 10 points) 


anotherSlice :: [[a]] -> (Int, Int) -> [Int] -> [[a]]
anotherSlice grid (r1,r2) lst = if r1 == r2 then []  else [(grid !! (r1) )!! c | c <- lst] : [] ++ anotherSlice grid (r1+1,r2) lst 

slice :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
slice grid (r1,r2) (c1,c2) =  if r1 == r2 then []  else [(grid !! (r1) )!! c | c <- [c1..c2-1]] : [] ++ slice grid (r1+1,r2)(c1,c2)

vcat :: [[a]] -> [[a]] -> [[a]]
vcat [] lst2 = lst2
vcat lst1 [] = lst1
vcat lst1 lst2 = head lst1 : [] ++ (vcat(tail lst1) lst2 )

hcat :: [[a]] -> [[a]] -> [[a]]
hcat [] lst2 = lst2
hcat lst1 [] = lst1
hcat lst1 lst2 = (head lst1 ++ head lst2) : (hcat (tail lst1) (tail lst2) )

without :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
--without grid (r1,r2) (c1,c2) = if (r1==r2 && c1==c2) then grid else if(r1==r2) then slice grid (0,length(grid)) (c2,length(grid!!0))  else if(c1==c2) then slice grid (r2,length(grid)) (0,length(grid !!0)) else slice grid (r2,length(grid)) (c2,length(grid!!0)) 
--without a (r1,r2) (c1,c2) = form( flatten ([(a !!i)!!j | i<-[0..(length (head a)-1)], i `notElem` [r1..r2-1], j<- [0..(length a - 1)], j `notElem` [c1..c2-1]] : []) ) ((length (head a)-r2+c1),(length a - c2+c1))
without grid (r1,r2) (c1,c2) = if r1==r2 && c1==c2 then grid else anotherSlice grid (0,r1) ([0..length(grid!!0)-1]\\[c1..(c2-1)]) ++ anotherSlice grid (r2,length(grid)) ([0..length(grid!!0)-1]\\[c1..(c2-1)])
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
