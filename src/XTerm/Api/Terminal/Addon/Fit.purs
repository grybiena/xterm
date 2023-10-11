module XTerm.Api.Terminal.Addon.Fit where

import Data.Unit (Unit)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import XTerm.Api.Disposable (class IsDisposable)

data FitAddon

foreign import fitAddon :: Effect FitAddon 

foreign import fit :: FitAddon -> Effect Unit

instance IsDisposable FitAddon where
  toDisposable = unsafeCoerce


