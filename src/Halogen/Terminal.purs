module Halogen.Terminal where

import Prelude

import Data.Array ((:))
import Data.Either (hush, isRight)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect.Aff (launchAff_, try)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.Terminal.Free (TerminalF(..))
import XTerm.Addons (fitAddon, webGLAddon, webLinksAddon)
import XTerm.Buffer (cursorX, cursorY, getLine, length)
import XTerm.Buffer.Namespace (active)
import XTerm.Buffer.Line as BL
import XTerm.Disposable (Disposable, dispose)
import XTerm.Terminal (Terminal, buffer, cols, element, loadAddon, onData, openTerminal, rows, textarea, write, writeln)

type State =
  { terminal :: Terminal
  , disposables :: Array Disposable
  }

data Action = 
    Initialize
  | Finalize
  | Raise Output

data Output =
  Data String

component :: forall m. MonadAff m => H.Component TerminalF Terminal Output m
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
             => TerminalF a
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
  ActiveBuffer a -> do
    { terminal } <- H.get
    pure $ Just $ a $ active $ buffer terminal
  CursorX b a -> do
    x <- H.liftEffect $ cursorX b
    pure $ Just $ a x
  CursorY b a -> do
    x <- H.liftEffect $ cursorY b
    pure $ Just $ a x
  BufferLength b a -> do
    x <- H.liftEffect $ length b
    pure $ Just $ a x
  GetBufferLine b l a -> do
    x <- H.liftEffect $ getLine b l
    pure $ Just $ a x
  BufferLineLength b a -> do
    x <- H.liftEffect $ BL.length b
    pure $ Just $ a x
  Write s a -> do
    { terminal } <- H.get
    sem <- H.liftAff $ AVar.empty
    H.liftEffect $ write terminal s (launchAff_ $ AVar.put unit sem)
    H.liftAff $ AVar.take sem
    pure (Just a)
  WriteLn s a -> do
    { terminal } <- H.get
    sem <- H.liftAff $ AVar.empty
    H.liftEffect $ writeln terminal s (launchAff_ $ AVar.put unit sem)
    H.liftAff $ AVar.take sem
    pure (Just a)
  FitAddon a -> do
    f <- H.liftEffect $ try fitAddon
    pure (Just $ a $ hush f)
  WebLinksAddon a -> do
    f <- H.liftEffect $ try webLinksAddon
    pure (Just $ a $ hush f)
  WebGLAddon a -> do
    f <- H.liftEffect $ try webGLAddon
    pure (Just $ a $ hush f)
  LoadAddon t a -> do
    { terminal } <- H.get
    f <- H.liftEffect $ try $ loadAddon terminal t
    pure (Just (a (isRight f)))


