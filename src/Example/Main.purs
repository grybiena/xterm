module Example.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (null, trim, length, toUpper)
import Data.String.CodeUnits (dropRight, takeRight)
import Data.Traversable (traverse, traverse_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.Buffer.Free (bufferLength, cursorX, getBufferLine, lineLength)
import Halogen.Shell (component)
import Halogen.Shell.Free (ShellM, getShell, interpreter, modifyShell, putShell, terminal)
import Halogen.Terminal as Terminal
import Halogen.Terminal.Free (TerminalM, loadAddon, webGLAddon, webLinksAddon, withActiveBuffer, write, writeLn)
import Halogen.VDom.Driver (runUI)



main :: Effect Unit
main = do
  HA.runHalogenAff do
     body <- HA.awaitBody
     let prompt = "$ "
         shell =
           { init: do
               terminal do
                 loadAddons true
                 write prompt
               interpreter (textInterpreter $ runRepl prompt (pure <<< toUpper))
           , query: const (pure unit)
           , shell: ""
           }
     runUI component shell body

loadAddons :: Boolean -> TerminalM Unit
loadAddons verbose = do
  wgl <- webGLAddon
  case wgl of
    Nothing -> when verbose $ writeLn "WebGL Addon unavailable"
    Just addon -> do
      when verbose $ write "Loading WebGL "
      ok <- loadAddon addon
      when verbose $ if ok then writeLn "OK" else writeLn "FAILED"
  wli <- webLinksAddon
  case wli of
    Nothing -> when verbose $ writeLn "WebLinks Addon unavailable"
    Just addon -> do
      when verbose $ write "Loading WebLinks "
      ok <- loadAddon addon
      when verbose $ if ok then writeLn "OK" else writeLn "FAILED"

textInterpreter :: forall o m .
            MonadEffect m
         => (String -> ShellM String o m Unit)
         -> Terminal.Output -> ShellM String o m Unit 
textInterpreter interpret =
  case _ of
    Terminal.Data d -> interpret d
    Terminal.LineFeed -> liftEffect $ log "line feed"
    _ -> pure unit


runRepl :: forall o m .
            MonadEffect m
         => String -> (String -> ShellM String o m String)
         -> String -> ShellM String o m Unit 
runRepl prompt repl =
  case _ of
    -- Ctrl+C
    "\x0003" -> do
       putShell ""
       terminal do
         write "^C"
         write ("\r\n" <> prompt)
    -- Enter
    "\r" -> do
       cmd <- getShell
       putShell ""
       res <- repl cmd
       terminal do
         when (not $ null $ trim cmd) $
           write ("\r\n" <> res) 
         write ("\r\n" <> prompt)
    -- BackSpace
    "\x007F" -> do
       cmd <- getShell
       terminal do
         x <- withActiveBuffer cursorX
         if (x == 0)
           then do
             write "\x1bM"
             ll <- withActiveBuffer do
                blen <- bufferLength
                blin <- getBufferLine (blen-1) 
                traverse lineLength blin
             write "\x9bK"
             flip traverse_ ll $ \l ->
               if (length cmd - 1 <= l)
                 then do
                    write prompt
                    write (dropRight 1 cmd)
                 else do
                    write (dropRight 1 $ takeRight l cmd)
            else
              when (length cmd > 0) do
                write "\x08 \x08"
       modifyShell (dropRight 1)
    -- Printable characters
    e | e >= "\x20" && e <= "\x7E" || e >= "\x00a0" -> do
      modifyShell (_ <> e)
      terminal $ write e
    e -> do
      liftEffect $ log $ "non-printable: " <> e
      pure unit
 
