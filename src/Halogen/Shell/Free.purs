module Halogen.Shell.Free where

import Prelude

import Control.Monad.Cont (class MonadTrans, lift)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Rec.Class (class MonadRec)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Terminal.Free (TerminalM)

data ShellF o m a =
    Terminal (TerminalM a)
  | Lift (m a)
  | GetCommand (String -> a)
  | PutCommand String a
  | Interpret (String -> ShellM o m Unit) a
  | Output o a

instance Functor m => Functor (ShellF o m) where
  map f (Terminal t) = Terminal (f <$> t)
  map f (Lift q) = Lift (f <$> q)
  map f (GetCommand s) = GetCommand (f <<< s)
  map f (PutCommand s a) = PutCommand s (f a)
  map f (Interpret s a) = Interpret s (f a)
  map f (Output o a) = Output o (f a)

type Shell o m = Free (ShellF o m)

newtype ShellM o m a = ShellM (Shell o m a)

instance MonadTrans (ShellM o) where
  lift = ShellM <<< liftF <<< Lift

instance MonadEffect m => MonadEffect (ShellM o m) where
  liftEffect = lift <<< liftEffect

instance MonadAff m => MonadAff (ShellM o m) where
  liftAff = lift <<< liftAff

derive newtype instance MonadRec (ShellM o m)
derive newtype instance Functor (ShellM o m)
derive newtype instance Apply (ShellM o m)
derive newtype instance Applicative (ShellM o m)
derive newtype instance Bind (ShellM o m)
derive newtype instance Monad (ShellM o m)

terminal :: forall o m a . TerminalM a -> ShellM o m a
terminal = ShellM <<< liftF <<< Terminal

getCommand :: forall o m . ShellM o m String
getCommand = ShellM $ liftF $ GetCommand identity 

putCommand :: forall o m . String -> ShellM o m Unit
putCommand s = ShellM $ liftF $ PutCommand s unit

modifyCommand :: forall o m . (String -> String) -> ShellM o m Unit
modifyCommand f = do
  cmd <- getCommand
  putCommand (f cmd)

interpret :: forall o m . (String -> ShellM o m Unit) -> ShellM o m Unit
interpret i = ShellM $ liftF $ Interpret i unit

output :: forall o m . o -> ShellM o m Unit
output o = ShellM $ liftF $ Output o unit

