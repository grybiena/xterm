module Halogen.Shell.Free where

import Prelude

import Control.Monad.Cont (class MonadTrans, lift)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Rec.Class (class MonadRec)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Terminal as Terminal
import Halogen.Terminal.Free (TerminalM)

data ShellF s o m a =
    Terminal (TerminalM a)
  | Lift (m a)
  | GetShell (s -> a)
  | PutShell s a
  | Interpreter (Terminal.Output -> ShellM s o m Unit) a
  | Output o a

instance Functor m => Functor (ShellF s o m) where
  map f (Terminal t) = Terminal (f <$> t)
  map f (Lift q) = Lift (f <$> q)
  map f (GetShell s) = GetShell (f <<< s)
  map f (PutShell s a) = PutShell s (f a)
  map f (Interpreter s a) = Interpreter s (f a)
  map f (Output o a) = Output o (f a)

type Shell s o m = Free (ShellF s o m)

newtype ShellM s o m a = ShellM (Shell s o m a)

instance MonadTrans (ShellM s o) where
  lift = ShellM <<< liftF <<< Lift

instance MonadEffect m => MonadEffect (ShellM s o m) where
  liftEffect = lift <<< liftEffect

instance MonadAff m => MonadAff (ShellM s o m) where
  liftAff = lift <<< liftAff

derive newtype instance MonadRec (ShellM s o m)
derive newtype instance Functor (ShellM s o m)
derive newtype instance Apply (ShellM s o m)
derive newtype instance Applicative (ShellM s o m)
derive newtype instance Bind (ShellM s o m)
derive newtype instance Monad (ShellM s o m)

terminal :: forall s o m a . TerminalM a -> ShellM s o m a
terminal = ShellM <<< liftF <<< Terminal

getShell :: forall s o m . ShellM s o m s
getShell = ShellM $ liftF $ GetShell identity 

putShell :: forall s o m . s -> ShellM s o m Unit
putShell s = ShellM $ liftF $ PutShell s unit

modifyShell :: forall s o m . (s -> s) -> ShellM s o m Unit
modifyShell f = do
  cmd <- getShell
  putShell (f cmd)

interpreter :: forall s o m . (Terminal.Output -> ShellM s o m Unit) -> ShellM s o m Unit
interpreter i = ShellM $ liftF $ Interpreter i unit

output :: forall s o m . o -> ShellM s o m Unit
output o = ShellM $ liftF $ Output o unit

