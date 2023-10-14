module Halogen.Terminal where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Array ((:))
import Data.Either (hush, isRight)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect.Aff (launchAff_, try)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.Buffer.Free (runBuffer)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.Terminal.Free (TerminalF(..))
import XTerm.Addons (fitAddon, webGLAddon, webLinksAddon)
import XTerm.Buffer.Namespace (active)
import XTerm.Disposable (Disposable, dispose)
import XTerm.Terminal (BinaryString, Key, RowRange, Terminal, ViewportSize, ViewportYOffset, buffer, cols, element, loadAddon, onBell, onBinary, onData, onKey, onLineFeed, onRender, onResize, onScroll, onSelectionChange, onTitleChange, onWriteParsed, openTerminal, rows, textarea, write, writeln)

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
  | Binary BinaryString
  | Bell
  | Key Key
  | LineFeed
  | Render RowRange
  | WriteParsed
  | Resize ViewportSize
  | Scroll ViewportYOffset
  | SelectionChange
  | TitleChange String

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
        outputBell terminal
        outputBinary terminal
        outputKey terminal
        outputLineFeed terminal
        outputRender terminal
        outputWriteParsed terminal
        outputResize terminal
        outputScroll terminal
        outputSelectionChange terminal
        outputTitleChange terminal
        outputData terminal
  Finalize -> do
    { disposables } <- H.get
    H.liftEffect $ traverse_ dispose disposables
  Raise o -> H.raise o
  where
    outputBell terminal = do
      { listener, emitter } <- H.liftEffect HS.create
      disposable <- H.liftEffect $ onBell terminal (HS.notify listener $ Raise $ Bell) 
      _ <- H.subscribe emitter 
      H.modify_ (\st -> st { disposables = disposable:st.disposables })
    outputBinary terminal = do
      { listener, emitter } <- H.liftEffect HS.create
      disposable <- H.liftEffect $ onBinary terminal (HS.notify listener <<< Raise <<< Binary) 
      _ <- H.subscribe emitter 
      H.modify_ (\st -> st { disposables = disposable:st.disposables })
    outputKey terminal = do
      { listener, emitter } <- H.liftEffect HS.create
      disposable <- H.liftEffect $ onKey terminal (HS.notify listener <<< Raise <<< Key) 
      _ <- H.subscribe emitter 
      H.modify_ (\st -> st { disposables = disposable:st.disposables })
    outputLineFeed terminal = do
      { listener, emitter } <- H.liftEffect HS.create
      disposable <- H.liftEffect $ onLineFeed terminal (HS.notify listener $ Raise $ LineFeed) 
      _ <- H.subscribe emitter 
      H.modify_ (\st -> st { disposables = disposable:st.disposables })
    outputRender terminal = do
      { listener, emitter } <- H.liftEffect HS.create
      disposable <- H.liftEffect $ onRender terminal (HS.notify listener <<< Raise <<< Render) 
      _ <- H.subscribe emitter 
      H.modify_ (\st -> st { disposables = disposable:st.disposables })
    outputWriteParsed terminal = do
      { listener, emitter } <- H.liftEffect HS.create
      disposable <- H.liftEffect $ onWriteParsed terminal (HS.notify listener $ Raise $ WriteParsed) 
      _ <- H.subscribe emitter 
      H.modify_ (\st -> st { disposables = disposable:st.disposables })
    outputResize terminal = do
      { listener, emitter } <- H.liftEffect HS.create
      disposable <- H.liftEffect $ onResize terminal (HS.notify listener <<< Raise <<< Resize) 
      _ <- H.subscribe emitter 
      H.modify_ (\st -> st { disposables = disposable:st.disposables })
    outputScroll terminal = do
      { listener, emitter } <- H.liftEffect HS.create
      disposable <- H.liftEffect $ onScroll terminal (HS.notify listener <<< Raise <<< Scroll) 
      _ <- H.subscribe emitter 
      H.modify_ (\st -> st { disposables = disposable:st.disposables })
    outputSelectionChange terminal = do
      { listener, emitter } <- H.liftEffect HS.create
      disposable <- H.liftEffect $ onSelectionChange terminal (HS.notify listener $ Raise $ SelectionChange) 
      _ <- H.subscribe emitter 
      H.modify_ (\st -> st { disposables = disposable:st.disposables })
    outputTitleChange terminal = do
      { listener, emitter } <- H.liftEffect HS.create
      disposable <- H.liftEffect $ onTitleChange terminal (HS.notify listener <<< Raise <<< TitleChange) 
      _ <- H.subscribe emitter 
      H.modify_ (\st -> st { disposables = disposable:st.disposables })
    outputData terminal = do
      { listener, emitter } <- H.liftEffect HS.create
      disposable <- H.liftEffect $ onData terminal (HS.notify listener <<< Raise <<< Data) 
      _ <- H.subscribe emitter 
      H.modify_ (\st -> st { disposables = disposable:st.disposables })



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
  WithActiveBuffer f -> do
    { terminal } <- H.get
    H.liftEffect $ Just <$> runReaderT (runBuffer f) (active $ buffer terminal)
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

