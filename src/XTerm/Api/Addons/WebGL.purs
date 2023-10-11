module XTerm.Api.Addons.WebGL where

import Data.Unit (Unit)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import XTerm.Api.Disposable (class IsDisposable)

data WebGLAddon

foreign import webGLAddon :: Effect WebGLAddon 

foreign import onContextLoss :: WebGLAddon -> (Event -> Effect Unit) -> Effect Unit

instance IsDisposable WebGLAddon where
  toDisposable = unsafeCoerce


