module AugsLink.Service.Room where

-- import Control.Monad.State

newtype State s a = State { runState :: s -> (a, s) }

data Room m = Room 
  {
    enter :: User -> m ()
  , present :: m [User]
  }

type Username = String

data User = User Username deriving Show

data RoomState = RoomState { connectedUsers :: [User] }

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

realRoom :: Room IO
realRoom = Room { enter = print }

modelRoom :: Room (State RoomState)
modelRoom = 
  Room { 
     enter = \u -> 
        State $ \state -> 
           ((), RoomState $ u : connectedUsers state) 

   , present = 
        State $ \s -> (connectedUsers s, s)
   }  

modelRoom' :: Room (State RoomState)
modelRoom' = 
  Room { 
     enter = \u -> 
       modify $ \s -> RoomState (u : connectedUsers s)

   , present = 
       get >>= (\s -> return (connectedUsers s))  
   }


modify ::  (s -> s) -> State s ()
modify stateChange = State $ \s -> ((), stateChange s)

get :: State s s
get = State (\s -> (s, s))

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) fb fa = (\a -> fa a >>= fb)

