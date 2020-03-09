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
without grid (r1,r2) (c1,c2) = if r1==r2 && c1==c2 then grid else anotherSlice grid (0,r1) ([0..length(grid!!0)-1]\\[c1..(c2-1)]) ++ anotherSlice grid (r2,length(grid)) ([0..length(grid!!0)-1]\\[c1..(c2-1)])
----------------------------
-- Return of the Non-trivial (30 points, 15 subject to runtime constraints)
--matchHelper grid value r c lst = if r == length(grid) then  matchHelper grid value (0) c+1 lst else if (grid !! r )!! c == value then matchHelper grid value (r+1) c ([(r,c)]++lst) else if r== length(grid) then [(5,5)] else undefined

--matchHelper grid value r c lst = if r == length(grid) && c < length (head grid) then  matchHelper grid value 0 (c+1) lst else if (grid !! r )!! c == value then matchHelper grid value (r+1) c ([(r,c)]++lst) else undefined
--matchHelper grid value r c = if r == length(grid) then [] else if (grid !! r )!! c == value then [(r,c)]++lst  else matchHelper grid value (r+1) c
matchHelper :: Eq a =>
                 [[a]] -> a -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
matchHelper grid value r c lst=
    if ((r== (length grid)-1) && (c == (length (head grid))-1) && (grid !! r )!! c == value ) --- 
        then ([(r,c)]++lst)
            else  if ((r== (length grid)-1) && (c == (length (head grid))-1) && (grid !! r )!! c /= value )
                then lst 
                    else if r == (length grid)  ---
                        then matchHelper grid value 0 (c+1) lst
                            else if (grid !! r )!! c == value
                                then matchHelper grid value (r+1) c ([(r,c)]++lst)  
                                    else matchHelper grid value (r+1) c lst
givefirst :: (Int ,Int) -> Int 
givefirst (x,_)= x
giveSecond :: (Int,Int) -> Int
giveSecond (_,y)= y

giveMeList :: Eq a => [[a]] -> a -> [(Int, Int)]
giveMeList grid value = matchHelper grid value 0 0 [] -- Gives [(2,2),(0,0)]

helperNew grid pattern lst outlist l r = 
    if l == r
        then outlist
            else if (givefirst(head(lst)) + length(pattern) > (length grid) || giveSecond(head lst) + length(head pattern) >length(head grid))
                then helperNew grid pattern (tail lst) outlist (l+1) r
                    else if (slice grid (givefirst(head(lst)), givefirst(head(lst))+length(pattern)) (giveSecond(head(lst)), giveSecond(head(lst)) +length(head pattern))) == pattern
                        then helperNew grid pattern (tail lst) ([(givefirst(head(lst)),giveSecond(head(lst)))]++outlist) (l+1) r
                            else helperNew grid pattern (tail lst)  outlist (l+1) r

matches2d :: Eq a => [[a]] -> [[a]] -> [(Int, Int)]
matches2d bigger smaller  = 
    if length (flatten(smaller)) == 1 
        then matchHelper bigger (flatten(smaller)!!0) 0 0 [] 
            else helperNew bigger smaller (giveMeList bigger ((smaller!!0)!!0) )  [] 0 (length(giveMeList bigger ((smaller!!0)!!0) ))
 
----------------------------
-- What is undefined? Just a value that will cause an error
-- when evaluated, from the GHC implementation:
-- undefined = error "Prelude.undefined"
-- But it allows your module to be compiled
-- since the function definitions will exist.
