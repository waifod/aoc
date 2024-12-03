{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import System.Environment
import qualified Data.Array as A
import Data.List 

-- L, Occupied, Floor
data Seat = L | O | F 
    deriving (Eq)

instance Show Seat where
    show L = "L"
    show O = "#"
    show F = "."

showRoom k =  intercalate "\n" $ (map (concat . map show . A.elems) . A.elems) k

-- u : up
-- b : below
-- r : right
-- l : left
-- ur, ul : up-right, up-left
-- br, bl : below-right, below-left 


neighbors arr x y = sum [u, r, l, b, ur, ul, br, bl]
    where (topu, topb) = A.bounds arr
          rows = A.elems arr
          (topl, topr) = A.bounds (head rows)
          u = check x (y-1)
          r = check (x+1) y
          l = check (x-1) y
          b = check x (y+1)
          ur = check (x+1) (y-1)
          ul = check (x-1) (y-1)
          br = check (x+1) (y+1)
          bl = check (x-1) (y+1)
          check k p = safecheck arr k p

safecheck arr x y = if safe then occ $ (arr A.! y) A.! x else 0
    where safe = y >= topu && y <= topb && x >= topl && x <= topr 
          (topu, topb) = A.bounds arr
          (topl, topr) = A.bounds (head (A.elems arr))

safeget arr x y = if safe then (arr A.! y) A.! x else F
    where safe = y >= topu && y <= topb && x >= topl && x <= topr 
          (topu, topb) = A.bounds arr
          (topl, topr) = A.bounds (head (A.elems arr))


queens arr x y = sum [u, r, l, b, ur, ul, br, bl]
    where (topu, topb) = A.bounds arr
          rows = A.elems arr
          (topl, topr) = A.bounds (head rows)
          u = check [(x, k) | k <- (reverse [topu..y-1])]
          r = check [(k, y) | k <- [x+1..topr]]
          l = check [(k, y) | k <- (reverse [topl..x-1])]
          b = check [(x, k) | k <- [y+1..topb]]
          ur = check $ zip [x+1..topr] (reverse [topu..y-1])
          ul = check $ zip (reverse [topl..x-1]) (reverse [topu..y-1])
          br = check $ zip [x+1..topr] [y+1..topb]
          bl = check $ zip (reverse [topl..x-1]) [y+1..topb]
    
          check [] = 0
          check ((x,y):ps) = case ((arr A.! y) A.! x) of 
            O -> 1
            L -> 0
            F -> check ps 

occ? O = True
occ? _ = False

occ O = 1
occ _ = 0

conv arr x y
    | pos == L = if count == 0 then O else L
    | pos == O = if count >= 4 then L else O
    | pos == F = F
    where count = neighbors arr x y
          pos = safeget arr x y

conv2 arr x y
    | pos == L = if count == 0 then O else L
    | pos == O = if count >= 5 then L else O
    | pos == F = F
    where count = queens arr x y
          pos = (arr A.! y) A.! x
        
updateRoom m = standardarr [standardarr [(conv m x y) | x <- [topl..topr]] | y <- [topu..topb]]
    where (topu, topb) = A.bounds m
          rows = A.elems m
          (topl, topr) = A.bounds (head rows)

updateRoom2 m = standardarr [standardarr [(conv2 m x y) | x <- [topl..topr]] | y <- [topu..topb]]
    where (topu, topb) = A.bounds m
          rows = A.elems m
          (topl, topr) = A.bounds (head rows)


debugn m = standardarr [standardarr [(neighbors m x y) | x <- [topl..topr]] | y <- [topu..topb]]
    where (topu, topb) = A.bounds m
          rows = A.elems m
          (topl, topr) = A.bounds (head rows)

debug2n m = standardarr [standardarr [(queens m x y) | x <- [topl..topr]] | y <- [topu..topb]]
    where (topu, topb) = A.bounds m
          rows = A.elems m
          (topl, topr) = A.bounds (head rows)

findStable m = if m == updateRoom m then m else findStable $ updateRoom m

findStable2 m = if m == updateRoom2 m then m else findStable2 $ updateRoom2 m
 
standardarr :: [a] -> A.Array Int a
standardarr k = A.listArray (0, size - 1) k
   where size = length k

range a k arr = [arr A.! x | x <- [a..k]]

-- Hell yeah ghetto parsing
parse ('.':xs) = F: parse xs
-- parse ('L':xs) = O: parse xs -- At the first iteration, everything becomes occupied.
parse ('L':xs) = L: parse xs -- Correct way
parse ('#':xs) = O: parse xs
parse _ = [] 

applyn 0 f a = a
applyn n f a = applyn (n-1) f (f a)



main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let seats = map (standardarr . parse) (lines content)
    let room = standardarr seats
    let res = findStable room
    let res2 = findStable2 room
    -- putStrLn $ showRoom $ applyn 0 updateRoom2 room
    -- putStrLn $ showRoom $ debug2n $ applyn 0 updateRoom2 room
    putStrLn $ "1:" ++ (show $ foldl (+) 0 (fmap (foldl (+) 0 . fmap occ) res))
    putStrLn $ "2:" ++ (show $ foldl (+) 0 (fmap (foldl (+) 0 . fmap occ) res2))