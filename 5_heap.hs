-- Author: Jordan Boulanger
-- CopyLEFT (Meant to be learned from)

-- Implmenting my own min heap in Haskell and continuing to practicing defining my own types

-- The 2nd value stores the number of nodes in the heap (from 'this' node down) 
-- aka it is the number of nodes in the tree or in a lower node the number of nodes in the subtree (counting the root)


-- Custom data type for the heap
data Heap a = Nil
              | Node a Int (Heap a) (Heap a)
              deriving(Eq, Show)


-- Counts the number of nodes in the current subtree
numNodesFromHere :: (Ord n) => Heap n -> Int
numNodesFromHere Nil = 0
numNodesFromHere (Node _ numNodes _ _) = numNodes

-- Given an element, creates a single element heap with that element inside
singleElementHeap :: (Ord a) => a -> Heap a
singleElementHeap elem = Node elem 1 Nil Nil

-- Merge two heaps
merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Nil t2 = t2
merge t1 Nil = t1
merge (Node val1 numNodes1 subL1 subR1) (Node val2 numNodes2 subL2 subR2)= 
  if  val1<val2 || (val1==val2 && numNodes1<=numNodes2)
  then if numNodesFromHere subL1 < numNodesFromHere subR1
       then Node val1 (numNodes1 + numNodes2) (merge subL1 (Node val2 numNodes2 subL2 subR2)) subR1
       else Node val1 (numNodes1 + numNodes2) subL1 (merge subR1 (Node val2 numNodes2 subL2 subR2))
  else merge (Node val2 numNodes2 subL2 subR2) (Node val1 numNodes1 subL1 subR1)

-- Inserts an element into a heap
insert :: (Ord a) => a -> Heap a -> Heap a
insert val heapin = merge heapin (singleElementHeap val)

-- gets the minimum value in the heap
getMin :: Heap a -> a
getMin Nil = error "The Heap is Empty"
getMin (Node val _ _ _) = val

-- delete the current minimum value from the heap
deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin Nil = Nil
deleteMin (Node val _ subL subR) = merge subL subR

-- build a heap from a given list of elements that can be ordered
buildHeap :: (Ord a) => [a] -> Heap a -> Heap a
buildHeap [] heapin = heapin
buildHeap [x] heapin = merge heapin (singleElementHeap x)
buildHeap (x:xs) heapin = buildHeap xs heap1
   where heap1 = merge (singleElementHeap x) heapin

-- Sort a given list using a heap
heapSort :: (Ord a) => [a] -> [a]
heapSort [] = []
heapSort (x:xs) = [item] ++ next (deleteMin heap)
  where heap = buildHeap (x:xs) Nil
        item = getMin heap
        next Nil = []
        next heap = [getMin heap] ++ next (deleteMin heap)