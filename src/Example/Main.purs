module Example.Main where

import Prelude

import Data.String (trim)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.Shell (component)
import Halogen.Shell.CommandLine (commandLine, runRepl, textInterpreter)
import Halogen.Shell.Free (interpreter, terminal)
import Halogen.Terminal.Free (cols, loadAddons, options, rows, write)
import Halogen.Terminal.Free.Options (getCursorBlink, getFontFamily, setCursorBlink)
import Halogen.VDom.Driver (runUI)



main :: Effect Unit
main = do
  HA.runHalogenAff do
     body <- HA.awaitBody
     let prompt = "$ "
         repl s | trim s == "rows" = show <$> terminal rows 
         repl s | trim s == "cols" = show <$> terminal cols
         repl s | trim s == "blink" = show <$> terminal (options getCursorBlink)
         repl s | trim s == "blinkon" = do
            terminal $ options $ setCursorBlink true
            pure ""
         repl s | trim s == "blinkoff" = do
            terminal $ options $ setCursorBlink false
            pure ""
         repl s | trim s == "fontFamily" = terminal $ options getFontFamily
         repl s = pure s
         shell =
           { init: do
               terminal do
                 loadAddons true
                 write prompt
               interpreter (textInterpreter $ commandLine (runRepl repl))
           , query: const (pure unit)
           , shell: (prompt /\ "")
           }
     runUI component shell body

