module Halogen.Shell.CommandLine where

import Prelude

import Data.Lens (Lens', _1, _2, (%~), (.~), (^.))
import Data.String (length, null, trim)
import Data.String.CodeUnits (dropRight, takeRight)
import Data.Traversable (traverse, traverse_)
import Data.Tuple.Nested (type (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Halogen.Buffer.Free (bufferLength, cursorX, getBufferLine, lineLength)
import Halogen.Shell.Free (ShellM, getShell, modifyShell, terminal)
import Halogen.Terminal.Free (withActiveBuffer, write)
import Halogen.Terminal as Terminal


class CommandLine shell where
  cmd :: Lens' shell String
  prompt :: Lens' shell String

instance CommandLine (String /\ String) where
  cmd = _2
  prompt = _1

commandLine :: forall shell o m .
            MonadEffect m
         => CommandLine shell
         => ShellM shell o m Unit 
         -> String -> ShellM shell o m Unit 
commandLine runCommand =
  case _ of
    -- Ctrl+C
    "\x0003" -> do
       modifyShell (cmd .~ "")
       shell <- getShell
       terminal do
         write "^C"
         write ("\r\n" <> (shell ^. prompt))
    -- Enter
    "\r" -> do
       runCommand
       shell <- getShell
       terminal do
         write ("\r\n" <> (shell ^. prompt))
    -- BackSpace
    "\x007F" -> backspace
    -- Printable characters
    e | e >= "\x20" && e <= "\x7E" || e >= "\x00a0" -> do
      modifyShell (cmd %~ (_ <> e))
      terminal $ write e
    e -> do
      liftEffect $ log $ "non-printable: " <> e
      pure unit
 
backspace :: forall shell o m .
            MonadEffect m
         => CommandLine shell
         => ShellM shell o m Unit 
backspace = do
  shell <- getShell
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
          if (length (shell ^. cmd) - 1 <= l)
            then do
               write (shell ^. prompt)
               write (dropRight 1 (shell ^. cmd))
            else do
               write (dropRight 1 $ takeRight l (shell ^. cmd))
       else
         when (length (shell ^. cmd) > 0) do
           write "\x08 \x08"
  modifyShell (cmd %~ dropRight 1)


textInterpreter :: forall shell o m .
            MonadEffect m
         => (String -> ShellM shell o m Unit)
         -> Terminal.Output -> ShellM shell o m Unit 
textInterpreter interpret =
  case _ of
    Terminal.Data d -> interpret d
    Terminal.LineFeed -> liftEffect $ log "line feed"
    _ -> pure unit

runRepl :: forall shell o m .
        MonadEffect m
     => CommandLine shell
     => (String -> ShellM shell o m String)
     -> ShellM shell o m Unit 
runRepl repl = do 
  shell <- getShell
  modifyShell (cmd .~ "")
  res <- repl (shell ^. cmd)
  terminal do
    when (not $ null $ trim (shell ^. cmd)) $
      write ("\r\n" <> res) 


