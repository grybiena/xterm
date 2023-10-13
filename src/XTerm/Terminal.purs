module XTerm.Terminal where

import Control.Alt (map, (<$>))
import Control.Category ((<<<))
import Data.Maybe (Maybe)
import Data.Options (Options, options)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (Unit)
import Effect (Effect)
import Foreign (Foreign)
import Web.DOM (Element)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import XTerm.Addons (class Addon, TerminalAddon, addon)
import XTerm.Buffer (BufferRange)
import XTerm.Buffer.Namespace (BufferNamespace)
import XTerm.Decoration (Decoration)
import XTerm.Decoration.Options (DecorationOptions, IDecorationOptions, makeDecorationOptions)
import XTerm.Disposable (Disposable)
import XTerm.LinkProvider (ILinkProvider, LinkProvider, makeLinkProvider)
import XTerm.Marker (Marker)
import XTerm.Modes (Modes)
import XTerm.Options (TerminalInitOnlyOptions, TerminalOptions)
import XTerm.Parser (Parser)
import XTerm.Utils (maybeUndefined)
import Xterm.LocalizableStrings (LocalizableStrings)
import Xterm.UnicodeHandling (UnicodeHandling)

data Terminal

foreign import _new :: Foreign -> Foreign -> Effect Terminal

new :: Options TerminalOptions -> Options TerminalInitOnlyOptions -> Effect Terminal
new o i = _new (options o) (options i)


foreign import _element :: Terminal -> Effect Foreign

element :: Terminal -> Effect (Maybe Element)
element = map maybeUndefined <<< _element

foreign import _textarea :: Terminal -> Effect Foreign

textarea :: Terminal -> Effect (Maybe Element)
textarea = map maybeUndefined <<< _textarea



foreign import rows :: Terminal -> Effect Int
foreign import cols :: Terminal -> Effect Int
foreign import buffer :: Terminal -> BufferNamespace
foreign import markers :: Terminal -> Effect (Array Marker)
foreign import parser :: Terminal -> Effect Parser
foreign import unicode :: Terminal -> Effect UnicodeHandling
foreign import modes :: Terminal -> Effect Modes

foreign import _setOptions :: Terminal -> Foreign -> Effect Unit

setOptions :: Terminal -> Options TerminalOptions -> Effect Unit 
setOptions t o = _setOptions t (options o)

foreign import localizableStrings :: Terminal -> Effect LocalizableStrings
foreign import onBell :: Terminal -> Effect Unit -> Effect Disposable
newtype BinaryString = BinaryString String
foreign import onBinary :: Terminal -> (BinaryString -> Effect Unit) -> Effect Disposable
foreign import onCursorMove :: Terminal -> Effect Unit -> Effect Disposable
foreign import onData :: Terminal -> (String -> Effect Unit) -> Effect Disposable
newtype KeycodeString = KeycodeString String
foreign import onKey :: Terminal -> ({ key :: KeycodeString, domEvent :: KeyboardEvent } -> Effect Unit) -> Effect Disposable
foreign import onLineFeed :: Terminal -> Effect Unit -> Effect Disposable
foreign import onRender :: Terminal -> ({start :: Int, end :: Int} -> Effect Unit) -> Effect Disposable
foreign import onWriteParsed :: Terminal -> Effect Unit -> Effect Disposable
foreign import onResize :: Terminal -> ({ cols :: Int, rows :: Int} -> Effect Unit) -> Effect Disposable
newtype ViewportYOffset = ViewportYOffset Int
foreign import onScroll :: Terminal -> (ViewportYOffset -> Effect Unit) -> Effect Disposable
foreign import onSelectionChange :: Terminal -> Effect Unit -> Effect Disposable
foreign import onTitleChange :: Terminal -> (String -> Effect Unit) -> Effect Disposable
foreign import blur :: Terminal -> Effect Unit
foreign import focus :: Terminal -> Effect Unit
foreign import resize :: Terminal -> { cols :: Int, rows :: Int} -> Effect Unit
foreign import openTerminal :: Terminal -> Element -> Effect Unit
foreign import attachCustomKeyEventHandler :: Terminal -> (KeyboardEvent -> Boolean) -> Effect Unit
foreign import registerILinkProvider :: Terminal -> ILinkProvider -> Effect Disposable

registerLinkProvider :: Terminal -> LinkProvider -> Effect Disposable
registerLinkProvider t p = registerILinkProvider t (makeLinkProvider p)

foreign import _registerCharacterJoiner :: Terminal -> (String -> Array (Array Int)) -> Effect JoinerId

registerCharacterJoiner :: Terminal -> (String -> Array (Int /\ Int)) -> Effect JoinerId
registerCharacterJoiner t j = _registerCharacterJoiner t (k <<< j)
  where k a = (\(x /\ y) -> [x,y]) <$> a

newtype JoinerId = JoinerId Int
foreign import deregisterCharacterJoiner :: Terminal -> JoinerId -> Effect Unit
newtype CursorYOffset = CursorYOffset Int
foreign import registerMarker :: Terminal -> CursorYOffset -> Effect Marker
foreign import _registerDecoration :: Terminal -> IDecorationOptions -> Effect Foreign -- (IDecoration \/ undefined)

registerDecoration :: Terminal -> DecorationOptions -> Effect (Maybe Decoration)
registerDecoration t o = maybeUndefined <$> _registerDecoration t (makeDecorationOptions o)

foreign import hasSelection :: Terminal -> Effect Boolean
foreign import getSelection :: Terminal -> Effect String
foreign import _getSelectionPosition :: Terminal -> Effect Foreign

getSelectionPosition :: Terminal -> Effect (Maybe BufferRange)
getSelectionPosition = map maybeUndefined <<< _getSelectionPosition

foreign import clearSelection :: Terminal -> Effect Unit
foreign import select :: Terminal -> { column :: Int, row :: Int, length :: Int } -> Effect Unit
foreign import selectAll :: Terminal -> Effect Unit
foreign import selectLines :: Terminal -> { start :: Int, end :: Int } -> Effect Unit
foreign import scrollLines :: Terminal -> Int -> Effect Unit
foreign import scrollPages :: Terminal -> Int -> Effect Unit
foreign import scrollToTop :: Terminal -> Effect Unit
foreign import scrollToBottom :: Terminal -> Effect Unit
foreign import scrollToLine :: Terminal -> Int -> Effect Unit
foreign import clear :: Terminal -> Effect Unit
foreign import write :: Terminal -> String -> Effect Unit -> Effect Unit
foreign import writeln :: Terminal -> String -> Effect Unit -> Effect Unit
foreign import paste :: Terminal -> String -> Effect Unit
foreign import refresh :: Terminal -> { start :: Int, end :: Int } -> Effect Unit
foreign import clearTextureAtlas :: Terminal -> Effect Unit
foreign import reset :: Terminal -> Effect Unit
foreign import _loadAddon :: Terminal -> TerminalAddon -> Effect Unit

loadAddon :: forall a . Addon a => Terminal -> a -> Effect Unit
loadAddon t a = _loadAddon t (addon a)



