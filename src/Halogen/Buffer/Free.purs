module Halogen.Buffer.Free where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect, liftEffect)
import XTerm.Buffer (Buffer, BufferType)
import XTerm.Buffer as X
import XTerm.Buffer.Line (BufferLine)



data BufferF a =
    BufferType (BufferType -> a)
  | CursorX (Int -> a)
  | CursorY (Int -> a)
  | BufferLength (Int -> a)
  | GetBufferLine Int (Maybe BufferLine -> a)

instance Functor BufferF where
  map f (BufferType e) = BufferType (f <<< e)
  map f (CursorX e) = CursorX (f <<< e)
  map f (CursorY e) = CursorY (f <<< e)
  map f (BufferLength e) = BufferLength (f <<< e)
  map f (GetBufferLine l e) = GetBufferLine l (f <<< e)

type BufferM = Free BufferF 

bufferType :: BufferM BufferType
bufferType = liftF $ BufferType identity

cursorX :: BufferM Int
cursorX = liftF $ CursorX identity

cursorY :: BufferM Int
cursorY = liftF $ CursorY identity

bufferLength :: BufferM Int 
bufferLength = liftF $ BufferLength identity

getBufferLine :: Int -> BufferM (Maybe BufferLine)
getBufferLine l = liftF $ GetBufferLine l identity

runBuffer :: forall m a . MonadEffect m => MonadRec m => BufferM a -> ReaderT Buffer m a
runBuffer = runFreeM go
  where
    go (BufferType a) = do
      b <- ask
      liftEffect $ a <$> X.bufferType b
    go (CursorX a) = do
      b <- ask
      liftEffect $ a <$> X.cursorX b
    go (CursorY a) = do
      b <- ask
      liftEffect $ a <$> X.cursorY b
    go (BufferLength a) = do
      b <- ask
      liftEffect $ a <$> X.length b
    go (GetBufferLine i a) = do
      b <- ask
      liftEffect $ a <$> X.getLine b i

