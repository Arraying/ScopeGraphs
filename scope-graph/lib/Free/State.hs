module Free.State where

import Free

data State s k 
  = Put s k 
  | Get (s -> k)

put :: State s < f => s -> Free f ()
put s = Do $ inj $ Put s $ Pure ()

get :: State s < f => Free f s
get = Do $ inj $ Get Pure

hState :: Functor f => Handler_ (State s) a s f a
hState = Handler_
  (const . return)
  (\x s -> case x of
    (Put s k) -> k s  
    (Get k) -> k s s)