-- Author: Jordan Boulanger
-- CopyLEFT (Meant to be learned from)

-- Practicing coding the function type prototype explicitly, so Haskell doesn't need to infer the type.
-- Defining the function prototype also helps with debugging, as we get an error if our function outputs a type we don't expect.
-- Notice we do this before the function definition.

-- Working with the built-in list type in Haskell
-- To practice I implemented some basic list functions myself
-- Most of these are already part of the Haskell standard library


-- Gets the minimum value in a list of values that can be ordered
myMin :: (Ord a) => [a] -> a
myMin xs = if null xs then error "No min list is empty" else if length xs == 1 then head xs 
	       else if head1 < head2 then (myMin (head1:newXS))
	       else myMin (head2:newXS)
	       where head1 = head xs
	             head2 = head (tail xs)
	             newXS = drop 2 xs

-- Reverses a list
myRev :: [a] -> [a]
myRev xs =if null xs then xs else ((myRev (tail xs)) ++ [head xs])

-- Gets the length of a list
myLen :: [a] -> Int
myLen xs = if null xs then 0 else myLen (tail xs) + 1

-- Checks if an element is in a list and returns True if so.		  
myElem :: (Eq a) => a -> [a] -> Bool
myElem y xs = if null xs then False else if head xs == y then True else (myElem y (tail xs))

-- Removes ALL instances of an element from a list
myRemElem :: (Eq a) => a -> [a] -> [a]
myRemElem y xs = if null xs then xs else if head xs == y then myRemElem y (tail xs) 
	             else [(head xs)] ++ (myRemElem y (tail xs))

-- Given a tuple, replaces all instances in the list of the first element in the tuple with the 2nd element the tuple
myRep1 :: (Eq a) => (a,a) -> [a] -> [a]
myRep1 (x, y) zs = if null zs then zs else if head zs == x then [y] ++ (myRep1 (x,y) (tail zs))  else  [head zs] ++ (myRep1 (x,y) (tail zs))  

-- Given a list of tuples, for each tuple, find all instances of the first value and substitues it with the 2nd value
-- Makes use of myRep1 function above, which takes a single tuple
-- For example, [(5,4), (6, 2)] replaces all elements in the list equal to 5 with 4 and all elements equal to 6 with 2
mySub :: (Eq a) => [(a,a)] -> [a] -> [a]
mySub xs ys =  if null xs then error "first list cant be null" else if null ys then ys
			  else if length xs == 1 then myRep1 (head xs) ys  else mySub (tail xs) (myRep1 (head xs) ys)

-- Param1: Int, Param2: List - gets sum of all instances of given int in the list
myElemSum :: Int -> [Int] -> Int
myElemSum y xs = if null xs then 0 else if head xs == y then y + (myElemSum y (tail xs)) 
	             else myElemSum y (tail xs)

-- Get an element at a given index
myAtIndex :: Int -> [a] -> a
myAtIndex y xs = if null xs then error "empty list" else if y > (length xs) - 1 
	             then error "Trying to get index greater than length of the list" 
	             else if y == 0 then head xs 
	             else myAtIndex (y-1) (tail xs)