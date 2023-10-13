module Halogen.Shell.Free where

import Prelude

import Control.Monad.Cont (class MonadTrans, lift)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Rec.Class (class MonadRec)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Terminal.Free (TerminalM)

data ShellF m a =
    Terminal (TerminalM a)
  | Lift (m a)
  | GetCommand (String -> a)
  | PutCommand String a

instance Functor m => Functor (ShellF m) where
  map f (Terminal t) = Terminal (f <$> t)
  map f (Lift q) = Lift (f <$> q)
  map f (GetCommand s) = GetCommand (f <<< s)
  map f (PutCommand s a) = PutCommand s (f a)

type Shell m = Free (ShellF m)

newtype ShellM m a = ShellM (Shell m a)

instance MonadTrans ShellM where
  lift = ShellM <<< liftF <<< Lift

instance MonadEffect m => MonadEffect (ShellM m) where
  liftEffect = lift <<< liftEffect

instance MonadAff m => MonadAff (ShellM m) where
  liftAff = lift <<< liftAff

derive newtype instance MonadRec (ShellM m)
derive newtype instance Functor (ShellM m)
derive newtype instance Apply (ShellM m)
derive newtype instance Applicative (ShellM m)
derive newtype instance Bind (ShellM m)
derive newtype instance Monad (ShellM m)

terminal :: forall m a . TerminalM a -> ShellM m a
terminal = ShellM <<< liftF <<< Terminal

getCommand :: forall m . ShellM m String
getCommand = ShellM $ liftF $ GetCommand identity 

putCommand :: forall m . String -> ShellM m Unit
putCommand s = ShellM $ liftF $ PutCommand s unit

modifyCommand :: forall m . (String -> String) -> ShellM m Unit
modifyCommand f = do
  cmd <- getCommand
  putCommand (f cmd)

