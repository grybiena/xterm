module XTerm.Api.Terminal.Addon.WebLinks where

import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import XTerm.Api.Disposable (class IsDisposable)

data WebLinksAddon

foreign import webLinksAddon :: Effect WebLinksAddon 

instance IsDisposable WebLinksAddon where
  toDisposable = unsafeCoerce


