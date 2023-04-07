-- Author: Jordan Boulanger
-- CopyLEFT (Meant to be learned from)

-- Implmenting my own linked list in Haskell and practicing defining my own types


-- Custom type defining a Node or Item in our linked list
data List a = Node a (List a)
  | Nil
  deriving (Eq, Show)

-- Inserts item into front of list
insert :: a -> List a -> List a  
insert x (Nil) = Node x (Nil)
insert x (Node y (tail)) = Node x (Node y (tail))


-- Deletes first occurance of item in list
delete :: (Eq a) => a -> List a -> List a
delete x (Nil) = Nil
delete x (Node y (tail))
  | x == y = tail
  | otherwise = Node y (delete x (tail))

-- Returns True if item is in the list
memberOf :: (Eq a) => a -> List a -> Bool
memberOf x (Nil) = False
memberOf x (Node y (tail)) 
  | x == y = True
  | otherwise = memberOf x (tail)

-- Returns the item at index i in the list
elementAt :: Int -> List a -> a 
elementAt i (Node y (tail))
  | i < 0 =  error "Negative Index"
  | i == 0 = y
  | otherwise = elementAt (i-1) (tail)

-- Inserts x at index i in the list (HELP NON EXAUSTIVE PATTERNS ERROR)
insertAt :: Int -> a -> List a -> List a
insertAt i x (Nil) 
  | i > 0 = error "out of bounds"

insertAt i x (Node y (tail)) 
  | i < 0 = error "negative index"
  | i == 0 =  Node x (Node y (tail))
  | otherwise = Node y (insertAt (i-1) x (tail))


-- deletes item at index i (HELP NON EXAUSTIVE PATTERNS ERROR)
deleteAt :: Int -> List a -> List a
deleteAt i (Nil) 
  | i > 0 = error "out of bounds"

deleteAt i (Node y (tail)) 
  | i < 0 = error "negative index"
  | i == 0 = tail
  | otherwise = Node y (deleteAt (i-1) (tail))


-- Appends the first list with the 2nd list
appendWith :: List a -> List a -> List a
appendWith (Nil) (y) = (y)
appendWith (x) (Nil) = (x)
appendWith (Node x (tail)) (y) = Node x (appendWith (tail) (y)) 

-- Returns the reverse of the List
reverseList :: List a -> List a
reverseList Nil = Nil
reverseList (Node x (tail)) = ((ReverseList tail) (Node x Nill) ) -- Help doesn't work how do i fix this logic?



-- Mimics zip function
zipList :: List a -> List b -> List (a,b)
zipList (x) Nil = error "1st list is longer"
zipList Nil (y) = error "2nd list is longer"
zipList (Node x (tail1)) (Node y (tail2)) = Node (x, y) (zipList (tail1) (tail2))


-- Mimics map function
mapList :: (a -> b) -> List a -> List b
mapList _ Nil = error "can't map an empty list"
mapList func (Node x (tail)) = Node (func x) (mapList func tail)


