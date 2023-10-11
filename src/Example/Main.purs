module Example.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Web.DOM (Document)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import XTerm.Api.Terminal (loadAddon, new, openTerminal, writeln)
import XTerm.Api.Addons (fit, fitAddon, webGLAddon, webLinksAddon)

foreign import onLoad :: (Document -> Effect Unit) -> Effect Unit

main :: Effect Unit
main = onLoad $ \doc -> do
  t <- getElementById "terminal" (toNonElementParentNode doc)
  case t of
    Nothing -> log "no terminal div"
    Just e -> do
      term <- new mempty mempty
      openTerminal term e
      writeln term "Hello there" (pure unit)
      gl <- webGLAddon
      loadAddon term gl
      writeln term "WebGL!!!" (pure unit)
      li <- webLinksAddon
      loadAddon term li
      writeln term "links https://duckduckgo.com" (pure unit)
      fi <- fitAddon
      loadAddon term fi
      fit fi
      writeln term "fit" (pure unit)



