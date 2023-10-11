module Test.Main where


import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Stream (createReadStream)
import Node.HTTP (Request, Response, createServer, listen, requestMethod, requestURL, responseAsStream, setHeader, setStatusCode, setStatusMessage)
import Node.Stream (end, pipe)

main :: Effect Unit
main = runTestServer 8080

runTestServer :: Int -> Effect Unit
runTestServer port = do
--  addrs@(AddressSpace { servers, httpServer, done }) <- newAddressSpace testServerPort
--  void $ execSync ("purs-nix bundle") defaultExecSyncOptions --{ stdio = inherit }

  let hostname = "127.0.0.1"
      serve req res =
        case requestMethod req of
          "GET" ->
            case split (Pattern "/") $ requestURL req of
              ["",""] -> serveTestPage req res
              ["","xterm.js"] -> serveXTermJs req res
              ["","xterm.js.map"] -> serveXTermJsMap req res
              ["","xterm.css"] -> serveXTermCSS req res
              ["","main.js"] -> serveMain req res
              _ -> errorRedirect req res
          _ -> errorRedirect req res
      serveTestPage req res = do
        setStatusOK req res
        let outputStream = responseAsStream res
        setHeader res "Content-Type" "text/html"
        f <- createReadStream ("./test/index.html")
        void $ pipe f outputStream
      serveXTermJs req res = do
        setStatusOK req res
        let outputStream = responseAsStream res
        setHeader res "Content-Type" "text/javascript"
        f <- createReadStream ("./node_modules/xterm/lib/xterm.js")
        void $ pipe f outputStream
      serveXTermJsMap req res = do
        setStatusOK req res
        let outputStream = responseAsStream res
        setHeader res "Content-Type" "text/json"
        f <- createReadStream ("./node_modules/xterm/lib/xterm.js.map")
        void $ pipe f outputStream
      serveMain req res = do
        setStatusOK req res
        let outputStream = responseAsStream res
        setHeader res "Content-Type" "text/javascript"
        f <- createReadStream ("./test/main.js")
        void $ pipe f outputStream
      serveXTermCSS req res = do
        setStatusOK req res
        let outputStream = responseAsStream res
        setHeader res "Content-Type" "text/css"
        f <- createReadStream ("./node_modules/xterm/css/xterm.css")
        void $ pipe f outputStream 
  server <- createServer serve

  listen server { backlog: Nothing, hostname, port } do
    log $ "Test HTTP server listening on port " <> show port <> "."
    pure unit


setStatusOK :: Request -> Response -> Effect Unit
setStatusOK req res = do
  setStatusCode res 200
  log $ "200 " <> (requestMethod req) <> " "  <> (requestURL req)

errorRedirect :: Request -> Response -> Effect Unit
errorRedirect req res = do
  setHeader res "X-Accel-Redirect" "/error_page.html"
  endWithStatus req res "error_page.html redirect" 200

endWithStatus :: Request -> Response -> String -> Int -> Effect Unit
endWithStatus _req res msg code = do 
--  log (requestMethod req <> " " <> requestURL req <> " " <> show code <> " " <> msg)
  setStatusMessage res msg
  setStatusCode res code
  let outputStream = responseAsStream res
  end outputStream (\_ -> pure unit)


