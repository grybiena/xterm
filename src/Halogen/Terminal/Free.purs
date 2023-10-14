module Halogen.Terminal.Free where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either (hush, isRight)
import Data.Maybe (Maybe)
import Effect.Aff (launchAff_, try)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen.Buffer.Free (BufferM, runBuffer)
import Web.DOM (Element)
import XTerm.Addons (class Addon, FitAddon, TerminalAddon, WebGLAddon, WebLinksAddon, addon)
import XTerm.Addons as XA
import XTerm.Buffer.Namespace as XB
import XTerm.Terminal (Terminal)
import XTerm.Terminal as XT


runTerminal :: forall m a .
               MonadAff m
            => MonadRec m
            => TerminalM a
            -> ReaderT Terminal m a
runTerminal = runFreeM go
  where
    go (TerminalElement a) = do
      terminal <- ask
      liftEffect $ a <$> XT.element terminal
    go (TextArea a) = do
      terminal <- ask
      liftEffect $ a <$> XT.textarea terminal
    go (Rows a) = do
      terminal <- ask
      liftEffect $ a <$> XT.rows terminal
    go (Cols a) = do
      terminal <- ask
      liftEffect $ a <$> XT.cols terminal
    go (WithActiveBuffer f) = do
      terminal <- ask
      liftEffect $ runReaderT (runBuffer f) (XB.active $ XT.buffer terminal)
    go (Write s a) = do
      terminal <- ask
      sem <- liftAff $ AVar.empty
      liftEffect $ XT.write terminal s (launchAff_ $ AVar.put a sem)
      liftAff $ AVar.take sem
    go (WriteLn s a) = do
      terminal <- ask
      sem <- liftAff $ AVar.empty
      liftEffect $ XT.writeln terminal s (launchAff_ $ AVar.put a sem)
      liftAff $ AVar.take sem
    go (FitAddon a) = do
      liftEffect $ (a <<< hush) <$> try XA.fitAddon
    go (WebLinksAddon a) = do
      liftEffect $ (a <<< hush) <$> try XA.webLinksAddon 
    go (WebGLAddon a) = do
      liftEffect $ (a <<< hush) <$> try XA.webGLAddon 
    go (LoadAddon t a) = do
       terminal <- ask
       liftEffect $ (a <<< isRight) <$> try (XT.loadAddon terminal t)

data TerminalF a =
    TerminalElement (Maybe Element -> a)
  | TextArea (Maybe Element -> a)
  | Rows (Int -> a)
  | Cols (Int -> a)
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
  map f (WithActiveBuffer b) = WithActiveBuffer (f <$> b)
  map f (Write s a) = Write s (f a)
  map f (WriteLn s a) = WriteLn s (f a)
  map f (FitAddon a) = FitAddon (f <<< a)
  map f (WebLinksAddon a) = WebLinksAddon (f <<< a)
  map f (WebGLAddon a) = WebGLAddon (f <<< a)
  map f (LoadAddon t a) = LoadAddon t (f <<< a)

type TerminalM = Free TerminalF 

terminalElement :: TerminalM (Maybe Element)
terminalElement = liftF $ TerminalElement identity

textArea :: TerminalM (Maybe Element)
textArea = liftF $ TextArea identity

rows :: TerminalM Int
rows = liftF $ Rows identity

cols :: TerminalM Int
cols = liftF $ Cols identity

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

