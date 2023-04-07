-- Author: Jordan Boulanger
-- CopyLEFT (Meant to be learned from)

-- Recoded my Haskell list functions to use pattern matching instead of if and else.
-- Notice the code is now a lot cleaner a lot cleaner and easier to read as well as shorter.
-- Practices using "otherwise"

-- Also includes a new function myElems that uses myElem to check if all elements in one list are in another


-- Gets the minimum value in a list of values that can be ordered
myMin :: (Ord a) => [a] -> a
myMin [] = error "No min list is empty"
myMin xs 
  | length xs == 1 = head xs 
  | head1 < head2 = (myMin (head1:newXS))
  | otherwise = myMin (head2:newXS)
  where 
    head1 = head xs
    head2 = head (tail xs)
    newXS = drop 2 xs

-- Reverses a list
myRev :: [a] -> [a]
myRev [] = []
myRev xs = myRev (tail xs) ++ [head xs]

-- Gets the length of a list
myLen :: [a] -> Int
myLen [] = 0
myLen xs = myLen (tail xs) + 1

-- Checks if an element is in a list and returns True if so.
myElem :: (Eq a) => a -> [a] -> Bool
myElem y [] = False 
myElem y xs
  | head xs == y = True
  | otherwise = (myElem y (tail xs))

-- Checks if all elements in the first list parameter are in the 2nd list
-- Uses myElem function
myElems :: (Eq a) => [a] -> [a] -> Bool
myElems [] xs = True
myElems (x:xs) ys = myElem x ys && myElems xs ys

-- Removes ALL instances of an element from a list
myRemElem :: (Eq a) => a -> [a] -> [a]
myRemElem y [] = [] 
myRemElem y xs 
  | head xs == y = myRemElem y (tail xs) 
  | otherwise = [(head xs)] ++ (myRemElem y (tail xs))


-- Given a tuple, replaces all instances in the list of the first element in the tuple with the 2nd element the tuple
myRep1 :: (Eq a) => (a,a) -> [a] -> [a]
myRep1 (x, y) [] = [] 
myRep1 (x, y) xs 
  | head xs == x = [y] ++ (myRep1 (x,y) (tail xs))
  | otherwise = [head xs] ++ (myRep1 (x,y) (tail xs))  


-- Given a list of tuples, for each tuple, find all instances of the first value and substitues it with the 2nd value
-- Makes use of myRep1 function above, which takes a single tuple
-- For example, [(5,4), (6, 2)] replaces all elements in the list equal to 5 with 4 and all elements equal to 6 with 2
mySub :: (Eq a) => [(a,a)] -> [a] -> [a]
mySub [] ys = ys
mySub (p:ps) ys = mySub ps (myRepl p ys)

-- Param1: Int, Param2: List - gets sum of all instances of given int in the list
myElemSum :: Int -> [Int] -> Int
myElemSum y [] = 0  
myElemSum y xs 
  | head xs == y = y + (myElemSum y (tail xs)) 
  | otherwise = myElemSum y (tail xs)

-- Get an element at a given index
myAtIndex :: Int -> [a] -> a
myAtIndex y [] = error "empty list"
myAtIndex y xs 
  | y > (length xs) - 1 || y < 0 = error "Trying to get index greater than length of the list or a negative index"
  | y == 0 = head xs 
  | otherwise = myAtIndex (y-1) (tail xs)