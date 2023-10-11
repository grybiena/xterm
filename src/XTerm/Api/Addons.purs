module XTerm.Api.Addons
  ( TerminalAddon
  , class Addon
  , addon
  , module XTerm.Api.Addons.Fit
  , module XTerm.Api.Addons.WebGL
  , module XTerm.Api.Addons.WebLinks
  ) where

import Unsafe.Coerce (unsafeCoerce)
import XTerm.Api.Addons.Fit (FitAddon, fit, fitAddon)
import XTerm.Api.Addons.WebGL (WebGLAddon, onContextLoss, webGLAddon)
import XTerm.Api.Addons.WebLinks (WebLinksAddon, webLinksAddon)

data TerminalAddon

class Addon a where
  addon :: a -> TerminalAddon 

instance Addon WebGLAddon where
  addon = unsafeCoerce

instance Addon FitAddon where
  addon = unsafeCoerce

instance Addon WebLinksAddon where
  addon = unsafeCoerce

