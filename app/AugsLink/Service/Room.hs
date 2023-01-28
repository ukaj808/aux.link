module AugsLink.Service.Room where

-- import Control.Monad.State

data Room m = Room 
  {
    enter :: User -> m ()
  , present :: m [User]
  }

model'' = Room (Integer)

inst :: Room IO
inst = Room { enter = print }



type Username = String

data User = User Username
  deriving Show

data RoomState = RoomState { connectedUsers :: [User] }



model :: Room (State RoomState)
model = 
  Room { 
         
      enter = \u -> State (\state -> ((), RoomState (u : connectedUsers state))) 
    , present = State (\s -> (connectedUsers s, s))
    }  

model' :: Room (State RoomState)
model' = Room 
           { 
               enter = \u -> modify (\s -> RoomState (u : connectedUsers s)) 
           ,   present = get >>= (\state -> return (connectedUsers state))  
           }



newtype State s a = State { runState :: s -> (a, s) }

modify ::  (s -> s) -> State s ()
modify stateChange = State (\state -> ((), stateChange state))

get :: State s s
get = State (\s -> (s, s))

{-
instance Monad (State s) where
  return :: a -> State s a
  return a = State $ (\s -> (a,s)) 
  
 
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) (State runStateA) fa = State (\st -> case runStateA st of (a, st') -> case fa a of State runStateB -> runStateB st')
          -- runStateB :: s -> (b,s)
    -- where runStateB = case stateAChangeResult of (a, st') -> (\st -
    --       stateAChangeResult :: (a ,s)
     --      stateAChangeResult = (runStateA st)
 

(>>=) :: m a -> (a -> m b) -> m b


(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) fb fa = (\a -> fa a >>= fb)

-}
