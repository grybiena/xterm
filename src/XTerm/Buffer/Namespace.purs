module XTerm.Buffer.Namespace where

import Data.Unit (Unit)
import Effect (Effect)
import XTerm.Buffer (Buffer)
import XTerm.Disposable (Disposable)

data BufferNamespace

foreign import active :: BufferNamespace -> Effect Buffer
foreign import normal :: BufferNamespace -> Effect Buffer
foreign import alternate :: BufferNamespace -> Effect Buffer
foreign import onBufferChange :: BufferNamespace -> (Buffer -> Effect Unit) -> Effect Disposable

