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
import Halogen.Terminal as Terminal
import Halogen.Terminal.Free (TerminalM)
import Type.Proxy (Proxy(..))
import XTerm.Options (cursorBlink, fontFamily)
import XTerm.Terminal (Terminal, new)

type Slots = ( terminal :: H.Slot TerminalM Terminal.Output Unit )

_terminal = Proxy :: Proxy "terminal"

type Shell q r o m =
  { init :: ShellM o m Unit
  , query :: q -> ShellM o m r
  }

type State q r o m =
  { shell :: Shell q r o m
  , interpreter :: Terminal.Output -> ShellM o m Unit
  , command :: String
  , terminal :: Maybe Terminal
  }

data Action =
    Initialize
  | TerminalOutput Terminal.Output

data Query q r a = Query q (r -> a)


component :: forall q r o m. MonadAff m => H.Component (Query q r) (Shell q r o m) o m
component = do
  H.mkComponent
    { initialState: \shell -> { shell, interpreter: const (pure unit), command: "", terminal: Nothing }
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
    Just te -> HH.slot _terminal unit Terminal.component te TerminalOutput

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
  TerminalOutput output -> do
     { interpreter } <- H.get
     void $ runShellM $ interpreter output

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
    go (Terminal f) = do
      r <- H.lift $ H.query _terminal unit f
      MaybeT $ pure r
    go (Lift m) = MaybeT $ Just <$> H.lift m
    go (GetCommand a) = do
      { command } <- H.lift H.get
      pure $ a command
    go (PutCommand c a) = do
      H.modify_ (\st -> st { command = c })
      pure a
    go (Interpreter i a) = do
      H.modify_ (\st -> st { interpreter = i })
      pure a
    go (Output o a) = do
      H.lift $ H.raise o
      pure a


