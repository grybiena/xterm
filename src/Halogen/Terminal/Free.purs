module Halogen.Terminal.Free where

import Control.Alt (class Functor, (<$>))
import Control.Category (identity, (<<<))
import Control.Monad.Free (Free, liftF)
import Data.Function (($))
import Data.Maybe (Maybe)
import Data.Unit (Unit, unit)
import Halogen.Buffer.Free (BufferM)
import Web.DOM (Element)
import XTerm.Addons (class Addon, FitAddon, TerminalAddon, WebGLAddon, WebLinksAddon, addon)
import XTerm.Buffer (Buffer)
import XTerm.Buffer.Line (BufferLine)



data TerminalF a =
    TerminalElement (Element -> a)
  | TextArea (Element -> a)
  | Rows (Int -> a)
  | Cols (Int -> a)
  | ActiveBuffer (Buffer -> a)
  | CursorX Buffer (Int -> a)
  | CursorY Buffer (Int -> a)
  | BufferLength Buffer (Int -> a)
  | GetBufferLine Buffer Int (Maybe BufferLine -> a)
  | BufferLineLength BufferLine (Int -> a)
  | WithActiveBuffer (BufferM a) 
  | Write String a
  | WriteLn String a
  | FitAddon (Maybe FitAddon -> a)
  | WebLinksAddon (Maybe WebLinksAddon -> a)
  | WebGLAddon (Maybe WebGLAddon -> a)
  | LoadAddon TerminalAddon (Boolean -> a)

instance Functor TerminalF where
  map f (TerminalElement e) = TerminalElement (f <<< e)
  map f (TextArea e) = TextArea (f <<< e)
  map f (Rows e) = Rows (f <<< e)
  map f (Cols e) = Cols (f <<< e)
  map f (ActiveBuffer e) = ActiveBuffer (f <<< e)
  map f (CursorX b e) = CursorX b (f <<< e)
  map f (CursorY b e) = CursorY b (f <<< e)
  map f (BufferLength b e) = BufferLength b (f <<< e)
  map f (GetBufferLine b l e) = GetBufferLine b l (f <<< e)
  map f (BufferLineLength b e) = BufferLineLength b (f <<< e)
  map f (WithActiveBuffer b) = WithActiveBuffer (f <$> b)
  map f (Write s a) = Write s (f a)
  map f (WriteLn s a) = WriteLn s (f a)
  map f (FitAddon a) = FitAddon (f <<< a)
  map f (WebLinksAddon a) = WebLinksAddon (f <<< a)
  map f (WebGLAddon a) = WebGLAddon (f <<< a)
  map f (LoadAddon t a) = LoadAddon t (f <<< a)

type TerminalM = Free TerminalF 

terminalElement :: TerminalM Element
terminalElement = liftF $ TerminalElement identity

textArea :: TerminalM Element
textArea = liftF $ TextArea identity

rows :: TerminalM Int
rows = liftF $ Rows identity

cols :: TerminalM Int
cols = liftF $ Cols identity

activeBuffer :: TerminalM Buffer
activeBuffer = liftF $ ActiveBuffer identity

cursorX :: Buffer -> TerminalM Int
cursorX b = liftF $ CursorX b identity

cursorY :: Buffer -> TerminalM Int
cursorY b = liftF $ CursorY b identity

bufferLength :: Buffer -> TerminalM Int 
bufferLength b = liftF $ BufferLength b identity

getBufferLine :: Buffer -> Int -> TerminalM (Maybe BufferLine)
getBufferLine b l = liftF $ GetBufferLine b l identity

bufferLineLength :: BufferLine -> TerminalM Int 
bufferLineLength b = liftF $ BufferLineLength b identity

withActiveBuffer :: forall a . BufferM a -> TerminalM a
withActiveBuffer b = liftF $ WithActiveBuffer b

write :: String -> TerminalM Unit
write s = liftF $ Write s unit

writeLn :: String -> TerminalM Unit
writeLn s = liftF $ WriteLn s unit

fitAddon :: TerminalM (Maybe FitAddon)
fitAddon = liftF $ FitAddon identity

webLinksAddon :: TerminalM (Maybe WebLinksAddon)
webLinksAddon = liftF $ WebLinksAddon identity

webGLAddon :: TerminalM (Maybe WebGLAddon)
webGLAddon = liftF $ WebGLAddon identity

loadAddon :: forall a . Addon a => a -> TerminalM Boolean
loadAddon t = liftF $ LoadAddon (addon t) identity

