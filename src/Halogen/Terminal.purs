module Halogen.Terminal where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Web.DOM (Element)
import XTerm.Buffer (cursorX)
import XTerm.Buffer.Namespace (active)
import XTerm.Disposable (Disposable, dispose)
import XTerm.Terminal (Terminal, buffer, cols, element, onData, openTerminal, rows, textarea, write, writeln)

type State =
  { terminal :: Terminal
  , disposables :: Array Disposable
  }

data Action = 
    Initialize
  | Finalize
  | Raise Output


data Query a =
    TerminalElement (Element -> a)
  | TextArea (Element -> a)
  | Rows (Int -> a)
  | Cols (Int -> a)
  | ActiveBufferCursorX (Int -> a)
  | Write String a
  | WriteLn String a


data Output =
  Data String

component :: forall m. MonadAff m => H.Component Query Terminal Output m
component = do
  H.mkComponent
    { initialState: \terminal -> { terminal, disposables: [] }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , handleQuery = handleQuery 
                                     , initialize = Just Initialize
                                     , finalize = Just Finalize
                                     }
    }


render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render _ = HH.div [ HP.ref (H.RefLabel "terminal") ] []

handleAction :: forall m .
                MonadAff m
             => Action
             -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    e <- H.getRef (H.RefLabel "terminal")
    case e of
      Nothing -> H.liftEffect $ log "no terminal element"
      Just el -> do
        { terminal } <- H.get
        H.liftEffect $ openTerminal terminal el
        datas <- H.liftEffect HS.create
        datad <- H.liftEffect $ onData terminal (HS.notify datas.listener <<< Raise <<< Data) 
        _ <- H.subscribe datas.emitter 
        H.modify_ (\st -> st { disposables = datad:st.disposables })
  Finalize -> do
    { disposables } <- H.get
    H.liftEffect $ traverse_ dispose disposables
  Raise o -> H.raise o


handleQuery :: forall m a .
                MonadAff m
             => Query a
             -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  TerminalElement a -> do
    { terminal } <- H.get
    e <- H.liftEffect $ element terminal 
    pure (a <$> e)
  TextArea a -> do
    { terminal } <- H.get
    e <- H.liftEffect $ textarea terminal 
    pure (a <$> e)
  Rows a -> do
    { terminal } <- H.get
    r <- H.liftEffect $ rows terminal
    pure (Just $ a r)
  Cols a -> do
    { terminal } <- H.get
    r <- H.liftEffect $ cols terminal
    pure (Just $ a r)
  ActiveBufferCursorX a -> do
    { terminal } <- H.get
    r <- H.liftEffect $ cursorX (active $ buffer terminal)
    pure (Just $ a r)
  Write s a -> do
    { terminal } <- H.get
    H.liftEffect $ write terminal s (pure unit)
    pure (Just a)
  WriteLn s a -> do
    { terminal } <- H.get
    H.liftEffect $ writeln terminal s (pure unit)
    pure (Just a)


