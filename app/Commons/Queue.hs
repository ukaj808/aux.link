module Commons.Queue
  ( 
    Queue (..)
  , BatchedQueue (..)
  ) where

class Queue q where
  qempty :: q a
  qisEmpty :: q a -> Bool
  enqueue :: q a -> a -> q a
  peek :: q a -> Maybe a
  dequeue :: q a -> Maybe (a, q a)
  qsize :: q a -> Int
  reorder :: q a -> [b] -> q a
  qremove  :: q a -> b -> q a

newtype BatchedQueue a = BatchedQueue ([a],[a])
  deriving Show




instance Queue BatchedQueue where

  peek :: BatchedQueue a -> Maybe a
  peek (BatchedQueue ([] , _)) = Nothing 
  peek (BatchedQueue (x:_, _)) = Just x 

  dequeue :: BatchedQueue a -> Maybe (a, BatchedQueue a)
  dequeue (BatchedQueue ([] , _)) = Nothing
  dequeue (BatchedQueue ([x], r)) = Just (x, BatchedQueue (reverse r, []))
  dequeue (BatchedQueue (x:f, r)) = Just (x, BatchedQueue (f        , r ))

  enqueue :: BatchedQueue a -> a -> BatchedQueue a
  enqueue (BatchedQueue ([],_)) x = BatchedQueue ([x], [] )
  enqueue (BatchedQueue (f ,r)) x = BatchedQueue (f  , x:r)

  qempty :: BatchedQueue a
  qempty = BatchedQueue ([],[])

  qisEmpty :: BatchedQueue a -> Bool
  qisEmpty (BatchedQueue ([],_)) = True
  qisEmpty _                     = False

  qsize :: BatchedQueue a -> Int
  qsize (BatchedQueue (f, r)) = length f + length r

  reorder :: BatchedQueue a -> [b] -> BatchedQueue a
  reorder = undefined
  
  qremove :: BatchedQueue a -> b -> BatchedQueue a
  qremove = undefined
