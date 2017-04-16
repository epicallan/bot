module Main where

import Control.Monad.State.Class
import App.Config.Config (FacebookStrategy, facebookStrategy)
import App.Foreign (cookieParser, expressSession, jsonBodyParser, morgan, passportInitialize, passportSession)
import Control.Monad.Aff (Aff, attempt, launchAff)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..))
import Data.List (List)
import Database.Mongo.Mongo (DB, Database, close, connect, Collection)
import Node.Express.App (App, get, listenHttp, post, useExternal)
import Node.Express.Handler (Handler)
import Node.Express.Response (send)
import Node.Express.Types (EXPRESS)
import Node.HTTP (Server)
import Node.Process (PROCESS, lookupEnv)
import Prelude (($), (<$>), (<>), (<<<), bind)

uri :: String
uri = "mongodb://127.0.0.1:27017/bot-test"

connectDB :: forall e. String -> Aff ( db :: DB | e) (Either Error Database)
connectDB dbUri =  attempt $ connect dbUri

appSetup :: forall e. Database -> App (console :: CONSOLE, db :: DB | e)
appSetup db =
    useExternal           morgan
    useExternal           cookieParser
    useExternal           jsonBodyParser
    useExternal           expressSession(db)
    useExternal           passportInitialize
    useExternal           passportSession
    post "/auth/fb"       (authHandler  db)
    get "/login/fb/return" authCallbackHandler -- succesful authentication

-- TODO Process exit handler to close mongodb process

main :: forall e. Eff (express :: EXPRESS, db :: DB, process :: PROCESS, console :: CONSOLE | e) Server
main = do
    port <- (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
    eitherDatabase <- connectDB uri
    case eitherDatabase of
      Left error -> liftEff $ logShow error -- app wont run
      Right database -> listenHttp (appSetup db) port \_ -> log $ "Listening on " <> port
