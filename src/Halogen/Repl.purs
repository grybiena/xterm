module Halogen.Repl where

import Prelude

import Data.Char (fromCharCode)
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.String.CodeUnits (dropRight, singleton, length)
import Data.Traversable (traverse_)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Terminal (Output(..), Query(..))
import Halogen.Terminal as Terminal
import Type.Proxy (Proxy(..))
import XTerm.Buffer (cursorX)
import XTerm.Buffer.Namespace (active)
import XTerm.Options (fontFamily)
import XTerm.Terminal (Terminal, buffer, new)

type Slots = ( terminal :: H.Slot Terminal.Query Terminal.Output Unit )

_terminal = Proxy :: Proxy "terminal"

type Repl =
  { prompt :: String
  , command :: String
  }

type State =
  { repl :: Repl
  , terminal :: Maybe Terminal
  }

data Action =
    Initialize
  | TerminalOutput Terminal.Output


component :: forall q o m. MonadAff m => H.Component q Repl o m
component = do
  H.mkComponent
    { initialState: \repl -> { repl, terminal: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render { terminal } =
  case terminal of
    Nothing -> HH.div_ []
    Just te -> HH.slot _terminal unit Terminal.component te TerminalOutput

handleAction :: forall o m .
                MonadAff m
             => Action
             -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    terminal <- H.liftEffect $ new (fontFamily := "\"Cascadia Code\", Menlo, monospace") mempty
    H.modify_ (\st -> st { terminal = Just terminal })
    st <- H.get
    H.tell _terminal unit (Write st.repl.prompt)
  TerminalOutput (Data d) -> runRepl d


runRepl :: forall o m .
        MonadAff m
     => String
     -> H.HalogenM State Action Slots o m Unit
runRepl "\x0003" = do -- Ctrl+C
  H.tell _terminal unit (Write "^C")
  H.modify_ (\st -> st { repl = st.repl { command = "" }})
  st <- H.get
  H.tell _terminal unit (Write ("\r\n" <> st.repl.prompt))
runRepl "\r" = do -- Enter
  st' <- H.get
  H.modify_ (\st -> st { repl = st.repl { command = "" }})
  -- TODO run the repl (echo for now)
  H.tell _terminal unit (Write ("\r\n" <> st'.repl.command))
  st <- H.get
  H.tell _terminal unit (Write ("\r\n" <> st.repl.prompt))
runRepl "\x007F" = do -- BackSpace
  H.liftEffect $ log "backspace"
  st' <- H.get
  -- TODO make this a query
  flip traverse_ st'.terminal $ \terminal -> do
    x <- H.liftEffect $ cursorX (active $ buffer terminal)
    when (x > length st'.repl.prompt) do
       H.tell _terminal unit (Write "\x08 \x08")
       H.modify_ (\st -> st { repl = st.repl { command = dropRight 1 st.repl.command }})
runRepl e | isPrintable e = do -- Print all printable characters
  H.liftEffect $ log $ "printable: \'" <> e <> "\'"
  H.modify_ (\st -> st { repl = st.repl { command = st.repl.command <> e }})
  H.tell _terminal unit (Write e)
runRepl e = do
  H.liftEffect $ log $ "non-printable: " <> e
  pure unit


isPrintable :: String -> Boolean
isPrintable e =
     (Just e >= (singleton <$> fromCharCode 32))
  && (Just e <= (singleton <$> fromCharCode 126))
  || (e >= "\\u00a0")


