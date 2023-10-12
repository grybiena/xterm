module Xterm.LocalizableStrings where

import Data.Unit (Unit)
import Effect (Effect)

data LocalizableStrings

foreign import getPromptLabel :: LocalizableStrings -> Effect String
foreign import setPromptLabel :: LocalizableStrings -> String -> Effect Unit

foreign import getTooMuchOutput :: LocalizableStrings -> Effect String
foreign import setTooMuchOutput :: LocalizableStrings -> String -> Effect Unit

