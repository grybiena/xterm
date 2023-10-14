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
import XTerm.Options (cursorBlink, fontFamily)
import XTerm.Terminal (Terminal, new)

type Slots = ( terminal :: H.Slot TerminalF T.Output Unit )

_terminal = Proxy :: Proxy "terminal"

type Shell q r o m =
  { init :: ShellM o m Unit
  , query :: q -> ShellM o m r
  }

type State q r o m =
  { shell :: Shell q r o m
  , interpret :: String -> ShellM o m Unit
  , command :: String
  , terminal :: Maybe Terminal
  }

data Action =
    Initialize
  | TerminalOutput T.Output

data Query q r a = Query q (r -> a)


component :: forall q r o m. MonadAff m => H.Component (Query q r) (Shell q r o m) o m
component = do
  H.mkComponent
    { initialState: \shell -> { shell, interpret: const (pure unit), command: "", terminal: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , handleQuery = handleQuery
                                     , initialize = Just Initialize
                                     }
    }

render :: forall q r o m. MonadAff m => State q r o m -> H.ComponentHTML Action Slots m
render { terminal } =
  case terminal of
    Nothing -> HH.div_ []
    Just te -> HH.slot _terminal unit T.component te TerminalOutput

handleAction :: forall q r o m .
                MonadAff m
             => Action
             -> H.HalogenM (State q r o m) Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    terminal <- H.liftEffect $ new (fontFamily := "\"Cascadia Code\", Menlo, monospace"
                                 <> cursorBlink := true
                                   ) mempty
    H.modify_ (\st -> st { terminal = Just terminal })
    { shell } <- H.get
    void $ runShellM $ shell.init
  TerminalOutput (Data d) -> do
     { interpret } <- H.get
     void $ runShellM $ interpret d
  TerminalOutput _ -> pure unit

handleQuery :: forall q r o m a .
                MonadAff m
             => Query q r a
             -> H.HalogenM (State q r o m) Action Slots o m (Maybe a)
handleQuery (Query q f) = do
  { shell } <- H.get
  r <- runShellM $ shell.query q
  pure $ f <$> r



runShellM :: forall q r o m a .
            MonadAff m
         => ShellM o m a
         -> H.HalogenM (State q r o m) Action Slots o m (Maybe a)
runShellM (ShellM s) = runMaybeT $ runFreeM go s
  where
    go (Terminal t) = MaybeT $ runTerminal t
    go (Lift m) = MaybeT $ Just <$> H.lift m
    go (GetCommand a) = do
      { command } <- H.lift H.get
      pure $ a command
    go (PutCommand c a) = do
      H.modify_ (\st -> st { command = c })
      pure a
    go (Interpret i a) = do
      H.modify_ (\st -> st { interpret = i })
      pure a
    go (Output o a) = do
      H.lift $ H.raise o
      pure a


runTerminal :: forall q r o m a .
               MonadAff m
            => TerminalM a
            -> H.HalogenM (State q r o m) Action Slots o m (Maybe a)
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
    go (WithActiveBuffer f) = do
      r <- H.lift $ H.query _terminal unit (WithActiveBuffer f)
      MaybeT $ pure r
    go (Write s a) = do
      H.lift $ H.tell _terminal unit (Write s)
      pure a
    go (WriteLn s a) = do
      H.lift $ H.tell _terminal unit (WriteLn s)
      pure a
    go (FitAddon a) = do
      r <- H.lift $ H.query _terminal unit (FitAddon a)
      MaybeT $ pure r 
    go (WebLinksAddon a) = do
      r <- H.lift $ H.query _terminal unit (WebLinksAddon a)
      MaybeT $ pure r
    go (WebGLAddon a) = do
      r <- H.lift $ H.query _terminal unit (WebGLAddon a)
      MaybeT $ pure r
    go (LoadAddon t a) = do
       r <- H.lift $ H.query _terminal unit (LoadAddon t identity)
       MaybeT $ pure $ a <$> r




