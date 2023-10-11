module XTerm.Addons
  ( TerminalAddon
  , class Addon
  , addon
  , module XTerm.Addons.Fit
  , module XTerm.Addons.WebGL
  , module XTerm.Addons.WebLinks
  ) where

import Unsafe.Coerce (unsafeCoerce)
import XTerm.Addons.Fit (FitAddon, fit, fitAddon)
import XTerm.Addons.WebGL (WebGLAddon, onContextLoss, webGLAddon)
import XTerm.Addons.WebLinks (WebLinksAddon, webLinksAddon)

data TerminalAddon

class Addon a where
  addon :: a -> TerminalAddon 

instance Addon WebGLAddon where
  addon = unsafeCoerce

instance Addon FitAddon where
  addon = unsafeCoerce

instance Addon WebLinksAddon where
  addon = unsafeCoerce

