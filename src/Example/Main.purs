module Example.Main where

import Prelude

import Data.String (toUpper)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.Repl (component)
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = do
  HA.runHalogenAff do
     body <- HA.awaitBody
     runUI component { prompt: "$ ", shell: pure <<< toUpper } body


