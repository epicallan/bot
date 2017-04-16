module Main where
import App.Foreign (jsonBodyParser)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Node.Express.App (App, listenHttp,get, useExternal)
import Node.Express.Handler (Handler)
import Node.Express.Response (send)
import Node.Express.Types (EXPRESS)
import Node.HTTP (Server)
import Prelude hiding (apply)


indexHandler :: forall e. Handler e
indexHandler = send "Make POST request with JSON body like {\"message\": <msg>} to get your message back"


appSetup :: forall e. App (console :: CONSOLE | e)
appSetup = do
    useExternal jsonBodyParser
    get  "/" indexHandler


main :: forall e. Eff (express :: EXPRESS, console :: CONSOLE | e) Server
main = do
    listenHttp appSetup 8080 \_ -> log $ "Listening on 8080"
