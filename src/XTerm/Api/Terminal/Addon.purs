module XTerm.Api.Terminal.Addon
  ( TerminalAddon
  , class Addon
  , addon
  , module XTerm.Api.Terminal.Addon.Fit
  , module XTerm.Api.Terminal.Addon.WebGL
  , module XTerm.Api.Terminal.Addon.WebLinks
  ) where

import Unsafe.Coerce (unsafeCoerce)
import XTerm.Api.Terminal.Addon.Fit (FitAddon, fit, fitAddon)
import XTerm.Api.Terminal.Addon.WebGL (WebGLAddon, onContextLoss, webGLAddon)
import XTerm.Api.Terminal.Addon.WebLinks (WebLinksAddon, webLinksAddon)

data TerminalAddon

class Addon a where
  addon :: a -> TerminalAddon 

instance Addon WebGLAddon where
  addon = unsafeCoerce

instance Addon FitAddon where
  addon = unsafeCoerce

instance Addon WebLinksAddon where
  addon = unsafeCoerce

