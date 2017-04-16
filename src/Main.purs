module Main where
import App.Foreign (cookieParser, expressSession, jsonBodyParser, morgan, passportInitialize, passportSession)
import Control.Monad.Aff (attempt, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Eff.Ref (REF,writeRef,newRef)
import Data.Either (Either(..))
import Database.Mongo.Mongo (DB, connect)
import Node.Express.App (App, listenHttp, get, useExternal)
import Node.Express.Handler (Handler)
import Node.Express.Response (send)
import Node.Express.Types (EXPRESS)
import Node.HTTP (Server)
import Prelude hiding (apply)
import App.Types (AppDb, DbRef)


uri :: String
uri = "mongodb://127.0.0.1:27017/bot-test"

initDbRef ::forall e. Eff (ref :: REF|e) DbRef
initDbRef = newRef $ Left (error "Mongodb is not connected")

addDbRef:: forall e. DbRef -> Eff (db :: DB, err :: EXCEPTION, ref :: REF, console :: CONSOLE | e) Unit
addDbRef dbRef = void $ launchAff do
  eitherDatabase <- attempt $ connect uri
  liftEff $ writeRef dbRef (eitherDatabase :: AppDb)
  liftEff $ log "added db"

indexHandler :: forall e. Handler e
indexHandler = send "Make POST request with JSON body like {\"message\": <msg>} to get your message back"

appSetup :: forall e. DbRef -> App (console :: CONSOLE, db :: DB, ref :: REF | e)
appSetup dbRef = do
    useExternal           morgan
    useExternal           cookieParser
    useExternal           jsonBodyParser
    useExternal           expressSession
    useExternal           passportInitialize
    useExternal           passportSession
    get  "/"              indexHandler
    -- post "/auth/fb"       (authHandler  db)
    -- get "/login/fb/return" authCallbackHandler -- succesful authentication

main :: forall e. Eff (express :: EXPRESS, db :: DB, err :: EXCEPTION, ref :: REF, console :: CONSOLE | e) Server
main = do
    dbRef <- initDbRef
    addDbRef dbRef
    listenHttp (appSetup dbRef) 8080 \_ -> log $ "Listening on 8080"
