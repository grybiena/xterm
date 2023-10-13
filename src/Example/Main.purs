module Example.Main where

import Prelude

import Data.String (toUpper)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.Shell.Free (runRepl)
import Halogen.Terminal.Shell (component)
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = do
  HA.runHalogenAff do
     body <- HA.awaitBody
     let prompt = "$ "
     runUI component { prompt, interpreter: runRepl prompt (pure <<< toUpper) } body


