module Main where
import App.Foreign as F
import App.Config (googleStrategy, jwtSecret)
import App.Handler.Messenger (messengerWebhookG, messengerWebhookP, verifyFbRequests)
import App.Handler.User (authHandler, indexHandler, addFbWebhook)
import App.Types (AppDb, DbRef, AppSetupEffs, AppEffs, ExitEffs)
import Control.Monad.Aff (attempt, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, info)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, message)
import Control.Monad.Eff.Ref (REF, newRef, writeRef)
import Data.Either (Either(..))
import Database.Mongo.Mongo (DB, connect)
import Messenger.Foreign (stopNgrok)
import Node.Express.App (get, listenHttp, post, useAt, useExternal, useOnError)
import Node.Express.Handler (Handler)
import Node.Express.Response (sendJson, setStatus)
import Node.HTTP (Server)
import Node.Process (onBeforeExit)
import Prelude hiding (apply)


uri :: String
uri = "mongodb://127.0.0.1:27017/bot-test"

initDbRef ::forall e. Eff (ref :: REF|e) DbRef
initDbRef = newRef $ Left (error "Mongodb is not connected")

addDbRef:: forall e. DbRef -> Eff (db :: DB, exception :: EXCEPTION, ref :: REF, console :: CONSOLE | e) Unit
addDbRef dbRef = void $ launchAff do
  eitherDatabase <- attempt $ connect uri
  liftEff $ writeRef dbRef (eitherDatabase :: AppDb)
  liftEff $ log "connected to db"

errorHandler :: forall e. Error -> Handler e
errorHandler err = do
  setStatus 400
  sendJson {error: message err}

onExit :: forall e. ExitEffs e
onExit = onBeforeExit $ void $ launchAff do
  liftEff $ info "Process is about to exit"
  liftEff stopNgrok

-- TODO i have no process related effect but i am required to add it here for program to compile
appSetup :: forall e. DbRef -> AppSetupEffs  e
appSetup dbRef = do
    useExternal                   F.morgan
    useExternal                   F.jsonBodyParser
    useExternal                   F.passportInitialize
    liftEff $                     F.googleAuthStrategy googleStrategy
    get "/"                       indexHandler
    useAt "/webhook/*"            verifyFbRequests
    post "/webhook/:userId"       $ messengerWebhookP dbRef
    get "/webhook/:userId"        messengerWebhookG
    get "/auth/google/"           F.googleAuth
    get "/auth/google/return"     $ F.googleAuthReturn authHandler dbRef
    useAt "/protected/*"          $ F.protectedRoutesHandler jwtSecret
    useAt "/protected/*"          F.setUserJwData
    get "/protected/user/webhook" $ addFbWebhook
    useOnError                    errorHandler

main :: forall e. AppEffs e Server
main = do
    dbRef <- initDbRef
    addDbRef dbRef
    onExit
    listenHttp (appSetup dbRef) 8080 \_ -> log $ "Listening on 8080"
