module Main where
import App.Foreign as F
import App.Config.Config (googleStrategy, jwtSecret)
import App.Foreign (PASSPORT)
import App.Handler.Messenger (messengerWebhook)
import App.Handler.User (authHandler, indexHandler, loginHandler, addFbWebhook)
import App.Types (AppDb, DbRef, AppSetupEffs, AppEffs)
import Control.Monad.Aff (attempt, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Eff.Ref (REF, newRef, writeRef)
import Data.Either (Either(..))
import Database.Mongo.Mongo (DB, connect)
import Node.Express.App (get, listenHttp, useAt, useExternal)
import Node.HTTP (Server)
import Prelude hiding (apply)


uri :: String
uri = "mongodb://127.0.0.1:27017/bot-test"

initDbRef ::forall e. Eff (ref :: REF|e) DbRef
initDbRef = newRef $ Left (error "Mongodb is not connected")

addDbRef:: forall e. DbRef -> Eff (db :: DB, err :: EXCEPTION, ref :: REF, console :: CONSOLE | e) Unit
addDbRef dbRef = void $ launchAff do
  eitherDatabase <- attempt $ connect uri
  liftEff $ writeRef dbRef (eitherDatabase :: AppDb)
  liftEff $ log "connected to db"

appSetup :: forall e. DbRef -> AppSetupEffs (passport :: PASSPORT | e)
appSetup dbRef = do
    useExternal               F.morgan
    useExternal               F.jsonBodyParser
    useExternal               F.passportInitialize
    liftEff $                 F.googleAuthStrategy googleStrategy
    get "/login"              loginHandler
    get "/"                   indexHandler
    get "/webhook/:userId"    messengerWebhook
    get "/auth/google/"       F.googleAuth
    get "/auth/google/return" (F.googleAuthReturn authHandler dbRef)
    useAt "/protected/*"      (F.protectedRoutesHandler jwtSecret)
    useAt "/protected/*"      (F.setUserJwData)
    get "/protected/user/webhook" addFbWebhook dbRef

main :: forall e. AppEffs (passport :: PASSPORT | e) Server
main = do
    dbRef <- initDbRef
    addDbRef dbRef
    listenHttp (appSetup dbRef) 8080 \_ -> log $ "Listening on 8080"
