-- Author: Jordan Boulanger
-- CopyLEFT (Meant to be learned from)

-- Some simple examples of Haskell working with inputs and lists


ascending3 x y z = if ((x < y) && (y < z)) then True else False

descending3 x y z = if ((x > y) && (y > z)) then True else False

unordered3 x y z = if not (descending3 x y z) && not (ascending3 x y z)  then True else False

distance (x, y) (x1, y1) = sqrt(((x1-x)^2) + ((y1-y)^2))

midpoint (x, y) (x1, y1) = (((x+x1)/2), ((y+y1)/2))

max3 x y z = if x > y then 
		     (if x > z then x else z) else (if y > z then y else z)

median3 x y z = if x < y then 
				(if y < z then y else (if z < y then z else y)) 
				else (if y < z then y else z)

quandrant (x, y) = if x >= 0 then (if y >= 0 then 1 else 4) else (if y >= 0 then 2 else 3)




ys = []                             
addElemToList x ys = ys ++ [x]
                                                            
xs = []
addFirstToLast xs = addElemToList (head xs) (tail xs)

addSecondToLast xs = [(head xs)] ++ (addFirstToLast (tail xs))

