module Example.Main where

import Prelude

import Data.String (toUpper)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.Repl (component)
import Halogen.VDom.Driver (runUI)





--main :: Effect Unit
--main = onLoad $ \doc -> do
--  t <- getElementById "terminal" (toNonElementParentNode doc)
--  case t of
--    Nothing -> log "no terminal div"
--    Just e -> do
--      term <- new mempty mempty
--      openTerminal term e
--      writeln term "Hello there" (pure unit)
--      gl <- webGLAddon
--      loadAddon term gl
--      writeln term "WebGL!!!" (pure unit)
--      li <- webLinksAddon
--      loadAddon term li
--      writeln term "links https://duckduckgo.com" (pure unit)
--      fi <- fitAddon
--      loadAddon term fi
--      fit fi
--      writeln term "fit" (pure unit)


main :: Effect Unit
main = do
  HA.runHalogenAff do
     body <- HA.awaitBody
     runUI component { prompt: "$ ", command: "", shell: pure <<< toUpper } body


