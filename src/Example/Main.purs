module Example.Main where

import Prelude

import Control.Monad.Cont (lift)
import Data.Maybe (Maybe(..))
import Data.String (null, trim, length, toUpper)
import Data.String.CodeUnits (dropRight, takeRight)
import Data.Traversable (traverse, traverse_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.Shell (component)
import Halogen.Shell.Free (ShellM, getCommand, modifyCommand, putCommand, terminal)
import Halogen.Terminal.Free (activeBuffer, bufferLength, bufferLineLength, cursorX, getBufferLine, loadAddon, webGLAddon, webLinksAddon, write, writeLn)
import Halogen.VDom.Driver (runUI)



main :: Effect Unit
main = do
  HA.runHalogenAff do
     body <- HA.awaitBody
     let prompt = "$ "
         shell = { configure: terminal do
                     wgl <- webGLAddon
                     case wgl of
                       Nothing -> writeLn "WebGL Addon unavailable"
                       Just addon -> do
                         writeLn "Loading WebGL Addon"
                         ok <- loadAddon addon
                         if ok then writeLn "  DONE" else writeLn "  FAILED"
                     wli <- webLinksAddon
                     case wli of
                       Nothing -> writeLn "WebLinks Addon unavailable"
                       Just addon -> do
                         writeLn "Loading WebLinks Addon"
                         ok <- loadAddon addon
                         if ok then writeLn "  DONE" else writeLn "  FAILED"
                     writeLn "Welcome to the internet"
                     write prompt
                 , interpret: runRepl prompt (pure <<< toUpper)
                 }
     runUI component shell body

runRepl :: forall m .
            MonadEffect m
         => String -> (String -> m String)
         -> String -> ShellM m Unit 
runRepl prompt repl =
  case _ of
    -- Ctrl+C
    "\x0003" -> do
       putCommand ""
       terminal do
         write "^C"
         write ("\r\n" <> prompt)
    -- Enter
    "\r" -> do
       cmd <- getCommand
       putCommand ""
       res <- lift $ repl cmd
       terminal do
         when (not $ null $ trim cmd) $
           write ("\r\n" <> res) 
         write ("\r\n" <> prompt)
    -- BackSpace
    "\x007F" -> do
       cmd <- getCommand
       terminal do
         buf <- activeBuffer
         x <- cursorX buf
         if (x == 0)
           then do
             write "\x1bM"
             blen <- bufferLength buf
             blin <- getBufferLine buf (blen-1) 
             ll <- traverse bufferLineLength blin
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
       modifyCommand (dropRight 1)
    -- Printable characters
    e | e >= "\x20" && e <= "\x7E" || e >= "\x00a0" -> do
      modifyCommand (_ <> e)
      terminal $ write e
    e -> do
      liftEffect $ log $ "non-printable: " <> e
      pure unit
 
