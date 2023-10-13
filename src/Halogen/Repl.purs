module Halogen.Repl where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.String (null, trim)
import Data.String.CodeUnits (dropRight, length)
import Data.Traversable (traverse_)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Terminal (Output(..), Query(..))
import Halogen.Terminal as Terminal
import Type.Proxy (Proxy(..))
import XTerm.Addons (webGLAddon, webLinksAddon)
import XTerm.Options (cursorBlink, fontFamily)
import XTerm.Terminal (Terminal, loadAddon, new)

type Slots = ( terminal :: H.Slot Terminal.Query Terminal.Output Unit )

_terminal = Proxy :: Proxy "terminal"

type Repl m =
  { prompt :: String
  , shell :: String -> m String
  }

type State m =
  { repl :: Repl m
  , command :: String
  , terminal :: Maybe Terminal
  }

data Action =
    Initialize
  | TerminalOutput Terminal.Output


component :: forall q o m. MonadAff m => H.Component q (Repl m) o m
component = do
  H.mkComponent
    { initialState: \repl -> { repl, command: "", terminal: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }

render :: forall m. MonadAff m => State m -> H.ComponentHTML Action Slots m
render { terminal } =
  case terminal of
    Nothing -> HH.div_ []
    Just te -> HH.slot _terminal unit Terminal.component te TerminalOutput

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
    st <- H.get
    H.tell _terminal unit (Write st.repl.prompt)
  TerminalOutput (Data d) -> runRepl d


runRepl :: forall o m .
        MonadAff m
     => String
     -> H.HalogenM (State m) Action Slots o m Unit
runRepl "\x0003" = do -- Ctrl+C
  H.tell _terminal unit (Write "^C")
  H.modify_ (\st -> st { command = "" })
  st <- H.get
  H.tell _terminal unit (Write ("\r\n" <> st.repl.prompt))
runRepl "\r" = do -- Enter
  st' <- H.get
  H.modify_ (\st -> st { command = "" })
  when (not $ null $ trim st'.command) do
    r <- H.lift $ st'.repl.shell st'.command
    H.tell _terminal unit (Write ("\r\n" <> r))
  st <- H.get
  H.tell _terminal unit (Write ("\r\n" <> st.repl.prompt))
runRepl "\x007F" = do -- BackSpace
  st' <- H.get
  blen <- H.query _terminal unit (ActiveBufferCursorX identity)
  flip traverse_ blen $ \x -> do
    when (x > length st'.repl.prompt) do
       H.tell _terminal unit (Write "\x08 \x08")
       H.modify_ (\st -> st { command = dropRight 1 st.command })
runRepl e | e >= "\x20" && e <= "\x7E" || e >= "\x00a0" = do -- Print all printable characters
  H.modify_ (\st -> st { command = st.command <> e })
  H.tell _terminal unit (Write e)
runRepl e = do
  H.liftEffect $ log $ "non-printable: " <> e
  pure unit

