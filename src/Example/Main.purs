module Example.Main where
import Example.Widget (component)
import Prelude

import Effect (Effect)
import Halogen.Aff as HA


import Halogen.VDom.Driver (runUI)



main :: Effect Unit
main = do
  HA.runHalogenAff do
     body <- HA.awaitBody
     runUI component unit body

