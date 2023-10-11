module Example.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Web.DOM (Document)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import XTerm.Api.Terminal (loadAddon, new, open, write, writeln)
import XTerm.Api.Terminal.Addon (webGlAddon)

foreign import onLoad :: (Document -> Effect Unit) -> Effect Unit

main :: Effect Unit
main = onLoad $ \doc -> do
  t <- getElementById "terminal" (toNonElementParentNode doc)
  case t of
    Nothing -> log "no terminal div"
    Just e -> do
      term <- new mempty mempty
      open term e
      writeln term "Hello there" (pure unit)
      mwgl <- webGlAddon
      case mwgl of
        Nothing -> write term "No WebGL" (pure unit)
        Just gl -> do
           loadAddon term gl
           write term "WebGL!!!" (pure unit)


