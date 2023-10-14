module Halogen.Terminal.Free.Options where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Rec.Class (class MonadRec)
import Effect.Class (class MonadEffect, liftEffect)
import XTerm.Options (TerminalOptions)
import XTerm.Options as XO

runOptions :: forall m a .
               MonadEffect m
            => MonadRec m
            => OptionsM a
            -> ReaderT TerminalOptions m a
runOptions = runFreeM go
  where
    go (GetCursorBlink f) = do
       opts <- ask
       liftEffect $ f <$> XO.getCursorBlink opts
    go (SetCursorBlink b a) = do
       opts <- ask
       liftEffect $ XO.setCursorBlink opts b
       pure a

data OptionsF a =
    GetCursorBlink (Boolean -> a)
  | SetCursorBlink Boolean a

type OptionsM = Free OptionsF

instance Functor OptionsF where
  map f (GetCursorBlink g) = GetCursorBlink (f <<< g)
  map f (SetCursorBlink b a) = SetCursorBlink b (f a)


getCursorBlink :: OptionsM Boolean
getCursorBlink = liftF $ GetCursorBlink identity

setCursorBlink :: Boolean -> OptionsM Unit
setCursorBlink b = liftF $ SetCursorBlink b unit
