module AugsLink.Core.Internal
  (
    modify
  , get
  , State (..)
  , Queue (..)
  ) where
import qualified Data.Text as T

newtype State s a = State { runState :: s -> (a, s) }

modify ::  (s -> s) -> State s ()
modify stateChange = State $ \s -> ((), stateChange s)

get :: State s s
get = State (\s -> (s, s))

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) fb fa a = fa a >>= fb

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f sa = State $ \s -> let (  a, s') = runState sa s
                            in  (f a, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  mf <*> ma = State $ \s -> let (f  , s'   ) = runState mf s
                                (  a, s''  ) = runState ma s'
                            in  (f a, s''  )

instance Monad (State s) where
  return :: a -> State s a
  return = pure


  (>>=) :: State s a -> (a -> State s b) -> State s b
  sa >>= k = State $ \s -> let (a, s') = runState sa s
                               sb      = k a
                           in runState sb s'

class Queue q where
  empty :: q a
  isEmpty :: q a -> Bool
  snoc :: q a -> a -> q a
  head :: q a -> Either T.Text a
  tail :: q a -> Either T.Text (q a) 

data BatchedQueue a = BatchedQueue 
  {
    f :: [a]
  , r :: [a]
  }

instance Queue BatchedQueue where

  head :: BatchedQueue a -> Either T.Text a
  head (BatchedQueue [] _) = Left $ T.pack "Queue is empty" 
  head (BatchedQueue (x : _) _) = Right x 

  tail :: BatchedQueue a -> Either T.Text (BatchedQueue a)
  tail (BatchedQueue [] _) = Left $ T.pack "Queue is empty"
  tail (BatchedQueue (_ : f) r) = Right $ BatchedQueue f r

  snoc :: BatchedQueue a -> a -> BatchedQueue a
  snoc q x = BatchedQueue (f q) (x : r q)

  empty :: BatchedQueue a
  empty = BatchedQueue{f=[], r=[]}

  isEmpty :: BatchedQueue a -> Bool
  isEmpty = undefined

