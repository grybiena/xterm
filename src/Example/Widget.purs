module Example.Widget where

import Prelude

import Data.Lens ((.~), (^.))
import Data.String (trim)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Example.Widget.Button as Button
import Halogen as H
import Halogen.HTML as HH
import Halogen.Shell as Shell
import Halogen.Shell.CommandLine (cmd, commandLine, prompt, runRepl, textInterpreter)
import Halogen.Shell.Free (ShellM, getShell, interpreter, modifyShell, output, terminal)
import Halogen.Terminal.Free (cols, loadAddons, options, rows, write)
import Halogen.Terminal.Free.Options (getCursorBlink, getFontFamily, setCursorBlink)
import Halogen.Terminal as Terminal


import Type.Proxy (Proxy(..))


type Slots = ( button :: forall q . H.Slot q Button.Output Unit
             , shell :: H.Slot (Shell.Query ShellQuery Unit) ShellOutput Unit
             )
_button = Proxy :: Proxy "button"
_shell = Proxy :: Proxy "shell"

data Action =
    ButtonClicked
  | ShellOutput ShellOutput

data ShellQuery = ButtonExit

data ShellOutput =
    MakeButton
  | RemoveButton

type State = { button :: Boolean }

component :: forall q i o m. MonadAff m => H.Component q i o m
component = do
  H.mkComponent
    { initialState: const { button: false }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render st =
  let proompt = "$ "
      repl s | trim s == "rows" = show <$> terminal rows 
      repl s | trim s == "cols" = show <$> terminal cols
      repl s | trim s == "blink" = show <$> terminal (options getCursorBlink)
      repl s | trim s == "blinkon" = do
         terminal $ options $ setCursorBlink true
         pure ""
      repl s | trim s == "blinkoff" = do
         terminal $ options $ setCursorBlink false
         pure ""
      repl s | trim s == "fontFamily" = terminal $ options getFontFamily
      repl s = pure s
      prog = do
        sh <- getShell
        if trim (sh ^. cmd) == "button"
          then do
             modifyShell (cmd .~ "")
             output MakeButton
             interpreter canceler
          else runRepl repl


      canceler :: Terminal.Output -> ShellM (String /\ String) ShellOutput m Unit
      canceler = textInterpreter $ (case _ of 
                                      -- Ctrl+C
                                      "\x0003" -> do
                                         output RemoveButton
                                         modifyShell (cmd .~ "")
                                         shell' <- getShell
                                         terminal do
                                           write "^C"
                                           write ("\r\n" <> (shell' ^. prompt))
                                         interpreter (textInterpreter $ commandLine prog)

                                      _ -> pure unit)


      shell =
        { init: do
            terminal do
              loadAddons true
              write proompt
            interpreter (textInterpreter $ commandLine prog)
        , query: \ButtonExit -> do
                     output RemoveButton
                     modifyShell (cmd .~ "")
                     shell' <- getShell
                     terminal do
                       write "click!"
                       write ("\r\n" <> (shell' ^. prompt))
                     interpreter (textInterpreter $ commandLine prog)
        , shell: (proompt /\ "")
        }

    in HH.div_
         [ HH.slot _shell unit Shell.component shell ShellOutput
         , if st.button
             then HH.slot _button unit Button.component unit (const ButtonClicked)
             else HH.div_ []
         ]


handleAction :: forall o m .
                MonadAff m
             => Action
             -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  ButtonClicked -> do
    H.modify_ (\st -> st { button = false })
    void $ H.query _shell unit (Shell.Query ButtonExit identity)
  ShellOutput MakeButton -> H.modify_ (\st -> st { button = true })
  ShellOutput RemoveButton -> H.modify_ (\st -> st { button = false })





