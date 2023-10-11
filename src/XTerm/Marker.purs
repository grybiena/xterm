module XTerm.Marker where

import Effect (Effect)

-- TODO extend to be IDisposableWithEvent 
data Marker

foreign import id :: Marker -> Int
foreign import line :: Marker -> Effect Int 
