module Halogen.Terminal.Free where

import Control.Alt (class Functor)
import Control.Category (identity, (<<<))
import Control.Monad.Free (Free, liftF)
import Data.Function (($))
import Data.Unit (Unit, unit)
import Web.DOM (Element)



data TerminalF a =
    TerminalElement (Element -> a)
  | TextArea (Element -> a)
  | Rows (Int -> a)
  | Cols (Int -> a)
  | ActiveBufferCursorX (Int -> a)
  | Write String a
  | WriteLn String a

instance Functor TerminalF where
  map f (TerminalElement e) = TerminalElement (f <<< e)
  map f (TextArea e) = TextArea (f <<< e)
  map f (Rows e) = Rows (f <<< e)
  map f (Cols e) = Cols (f <<< e)
  map f (ActiveBufferCursorX e) = ActiveBufferCursorX (f <<< e)
  map f (Write s a) = Write s (f a)
  map f (WriteLn s a) = WriteLn s (f a)

type TerminalM = Free TerminalF 

terminalElement :: TerminalM Element
terminalElement = liftF $ TerminalElement identity

textArea :: TerminalM Element
textArea = liftF $ TextArea identity

rows :: TerminalM Int
rows = liftF $ Rows identity

cols :: TerminalM Int
cols = liftF $ Cols identity

activeBufferCursorX :: TerminalM Int
activeBufferCursorX = liftF $ ActiveBufferCursorX identity

write :: String -> TerminalM Unit
write s = liftF $ Write s unit


writeLn :: String -> TerminalM Unit
writeLn s = liftF $ WriteLn s unit



