module Commons.Queue
  ( 
    Queue (..)
  , BatchedQueue (..)
  ) where

class Queue q where
  qempty :: q a
  qisEmpty :: q a -> Bool
  qadd :: q a -> a -> q a
  qpeek :: q a -> Maybe a
  qpoll :: q a -> Maybe (a, q a)
  qsize :: q a -> Int

newtype BatchedQueue a = BatchedQueue ([a],[a])
  deriving Show

instance Queue BatchedQueue where

  qpeek :: BatchedQueue a -> Maybe a
  qpeek (BatchedQueue ([] , _)) = Nothing 
  qpeek (BatchedQueue (x:_, _)) = Just x 

  qpoll :: BatchedQueue a -> Maybe (a, BatchedQueue a)
  qpoll (BatchedQueue ([] , _)) = Nothing
  qpoll (BatchedQueue ([x], r)) = Just (x, BatchedQueue (reverse r, []))
  qpoll (BatchedQueue (x:f, r)) = Just (x, BatchedQueue (f        , r ))

  qadd :: BatchedQueue a -> a -> BatchedQueue a
  qadd (BatchedQueue ([],_)) x = BatchedQueue ([x], [] )
  qadd (BatchedQueue (f ,r)) x = BatchedQueue (f  , x:r)

  qempty :: BatchedQueue a
  qempty = BatchedQueue ([],[])

  qisEmpty :: BatchedQueue a -> Bool
  qisEmpty (BatchedQueue ([],_)) = True
  qisEmpty _                     = False

  qsize :: BatchedQueue a -> Int
  qsize (BatchedQueue (f, r)) = length f + length r
