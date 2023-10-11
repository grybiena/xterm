module XTerm.Api.Terminal where

import Control.Alt (map)
import Control.Category ((<<<))
import Data.Maybe (Maybe)
import Data.Options (Options, options)
import Data.Unit (Unit)
import Effect (Effect)
import Foreign (Foreign)
import Web.DOM (Element)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import XTerm.Api.Buffer (BufferRange)
import XTerm.Api.Buffer.Namespace (BufferNamespace)
import XTerm.Api.Disposable (Disposable)
import XTerm.Api.LinkProvider (ILinkProvider, LinkProvider, makeLinkProvider)
import XTerm.Api.Marker (Marker)
import XTerm.Api.Options (TerminalInitOnlyOptions, TerminalOptions)
import XTerm.Api.Addons (class Addon, TerminalAddon, addon)
import XTerm.Api.Utils (maybeUndefined)

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
foreign import buffer :: Terminal -> Effect BufferNamespace
foreign import markers :: Terminal -> Effect (Array Marker)
--foreign import parser :: Terminal -> Effect IParser
--foreign import unicode :: Terminal -> Effect IUnicodeHandling
--foreign import modes :: Terminal -> Effect IModes

foreign import _setOptions :: Terminal -> Foreign -> Effect Unit

setOptions :: Terminal -> Options TerminalOptions -> Effect Unit 
setOptions t o = _setOptions t (options o)

--foreign import strings :: Terminal -> Effect ILocalizableStrings
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

--foreign import registerCharacterJoiner :: Terminal -> (String -> Array (Int /\ Int)) -> Effect JoinerId
--foreign import deregisterCharacterJoiner :: Terminal -> JoinerId -> Effect Unit
newtype CursorYOffset = CursorYOffset Int
foreign import registerMarker :: Terminal -> CursorYOffset -> Effect Marker
--foreign import registerDecoration :: Terminal -> IDecorationOptions -> Effect (IDecoration \/ undefined)
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



