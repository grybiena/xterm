module XTerm.Api.Terminal.Addon
  ( TerminalAddon
  , class Addon
  , addon
  , module XTerm.Api.Terminal.Addon.WebGL
  ) where

import Unsafe.Coerce (unsafeCoerce)
import XTerm.Api.Terminal.Addon.WebGL

data TerminalAddon

class Addon a where
  addon :: a -> TerminalAddon 

instance Addon WebGLAddon where
  addon = unsafeCoerce

