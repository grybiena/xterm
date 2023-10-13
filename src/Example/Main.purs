module Example.Main where

import Prelude

import Control.Monad.Cont (lift)
import Data.String (null, trim, length, toUpper)
import Data.String.CodeUnits (dropRight)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.Shell (component)
import Halogen.Shell.Free (ShellM, getCommand, modifyCommand, putCommand, terminal)
import Halogen.Terminal.Free (activeBufferCursorX, write)
import Halogen.VDom.Driver (runUI)



main :: Effect Unit
main = do
  HA.runHalogenAff do
     body <- HA.awaitBody
     let prompt = "$ "
         shell = { configure: terminal do
                     write "hello there\r\n"
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
       terminal do
         x <- activeBufferCursorX
         when (x > length prompt) do
           write "\x08 \x08"
       modifyCommand (dropRight 1)
    -- Printable characters
    e | e >= "\x20" && e <= "\x7E" || e >= "\x00a0" -> do
      modifyCommand (_ <> e)
      terminal $ write e
    e -> do
      liftEffect $ log $ "non-printable: " <> e
      pure unit
 
