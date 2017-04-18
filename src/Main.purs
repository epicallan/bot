module Main where
import App.Foreign as F
import App.Config.Config (facebookStrategy)
import App.Foreign (PASSPORT)
import App.Handler.User (authHandler, loginHandler)
import App.Types (AppDb, DbRef, SessionOptions, AppSetupEffs, AppEffs)
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


sessionOptions = { mongoUri: uri, secret: "Your cat" } :: SessionOptions

appSetup :: forall e. DbRef -> AppSetupEffs (passport :: PASSPORT | e)
appSetup dbRef = do
    useExternal             F.morgan
    useExternal             F.cookieParser
    useExternal             F.jsonBodyParser
    useExternal             (F.expressSession sessionOptions)
    useExternal             F.passportInitialize
    liftEff $               F.facebookAuthStrategy facebookStrategy
    get "/login"            loginHandler -- will redirect to login page
    get "/auth/fb/"         F.facebookAuth
    useAt "/auth/fb/return" (F.facebookAuthReturn dbRef authHandler)


main :: forall e. AppEffs (passport :: PASSPORT | e) Server
main = do
    dbRef <- initDbRef
    addDbRef dbRef
    listenHttp (appSetup dbRef) 8080 \_ -> log $ "Listening on 8080"
