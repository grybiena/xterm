module XTerm.Api.Terminal.Addon where

import Control.Alt ((<$>))
import Data.Maybe (Maybe)
import Effect (Effect)
import Foreign (Foreign)
import XTerm.Api.Utils (maybeUndefined)

data TerminalAddon

foreign import _webGlAddon :: Effect Foreign 

webGlAddon :: Effect (Maybe TerminalAddon)
webGlAddon = maybeUndefined <$> _webGlAddon
