module Halogen.Shell where

import Prelude

import Control.Monad.Free (runFreeM)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Shell.Free (ShellF(..), ShellM(..))
import Halogen.Terminal (Output(..))
import Halogen.Terminal as T
import Halogen.Terminal.Free (TerminalF(..), TerminalM)
import Type.Proxy (Proxy(..))
import XTerm.Addons (webGLAddon, webLinksAddon)
import XTerm.Options (cursorBlink, fontFamily)
import XTerm.Terminal (Terminal, loadAddon, new)

type Slots = ( terminal :: H.Slot TerminalF T.Output Unit )

_terminal = Proxy :: Proxy "terminal"

type Shell m =
  { configure :: ShellM m Unit
  , interpret :: String -> ShellM m Unit
  }

type State m =
  { shell :: Shell m
  , command :: String
  , terminal :: Maybe Terminal
  }

data Action =
    Initialize
  | TerminalOutput T.Output


component :: forall q o m. MonadAff m => H.Component q (Shell m) o m
component = do
  H.mkComponent
    { initialState: \shell -> { shell, command: "", terminal: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }

render :: forall m. MonadAff m => State m -> H.ComponentHTML Action Slots m
render { terminal } =
  case terminal of
    Nothing -> HH.div_ []
    Just te -> HH.slot _terminal unit T.component te TerminalOutput

handleAction :: forall o m .
                MonadAff m
             => Action
             -> H.HalogenM (State m) Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    terminal <- H.liftEffect $ new (fontFamily := "\"Cascadia Code\", Menlo, monospace"
                                 <> cursorBlink := true
                                   ) mempty
    H.liftEffect do
      gl <- webGLAddon
      loadAddon terminal gl
      li <- webLinksAddon
      loadAddon terminal li
    H.modify_ (\st -> st { terminal = Just terminal })
    { shell } <- H.get
    void $ runShellM $ shell.configure
  TerminalOutput (Data d) -> do
     { shell } <- H.get
     void $ runShellM $ shell.interpret d

runShellM :: forall o m a .
            MonadAff m
         => ShellM m a
         -> H.HalogenM (State m) Action Slots o m (Maybe a)
runShellM (ShellM s) = runMaybeT $ runFreeM go s
  where
    go (Terminal t) = MaybeT $ runTerminal t
    go (Lift m) = MaybeT $ Just <$> H.lift m
    go (GetCommand a) = do
      { command } <- H.lift H.get
      MaybeT $ pure $ Just $ a command
    go (PutCommand c a) = do
      H.modify_ (\st -> st { command = c })
      MaybeT $ pure $ Just a 


runTerminal :: forall o m a .
               MonadAff m
            => TerminalM a
            -> H.HalogenM (State m) Action Slots o m (Maybe a)
runTerminal = runMaybeT <<< runFreeM go
  where
    go (TerminalElement a) = do
      r <- H.lift $ H.query _terminal unit (TerminalElement identity)
      MaybeT $ pure $ a <$> r
    go (TextArea a) = do
      r <- H.lift $ H.query _terminal unit (TextArea identity)
      MaybeT $ pure $ a <$> r
    go (Rows a) = do
      r <- H.lift $ H.query _terminal unit (Rows identity)
      MaybeT $ pure $ a <$> r
    go (Cols a) = do
      r <- H.lift $ H.query _terminal unit (Cols identity)
      MaybeT $ pure $ a <$> r
    go (ActiveBufferCursorX a) = do
      r <- H.lift $ H.query _terminal unit (ActiveBufferCursorX identity)
      MaybeT $ pure $ a <$> r
    go (Write s a) = do
      H.lift $ H.tell _terminal unit (Write s)
      pure a
    go (WriteLn s a) = do
      H.lift $ H.tell _terminal unit (WriteLn s)
      pure a

