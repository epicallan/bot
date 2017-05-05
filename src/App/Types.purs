module App.Types where
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Control.Monad.Eff.Ref (REF, Ref)
import Data.Either (Either)
import Database.Mongo.Mongo (DB, Database)
import Messenger.Foreign (Ngrok)
import Network.HTTP.Affjax (AJAX)
import Node.Express.App (App)
import Node.Express.Handler (Handler)
import Node.Express.Types (EXPRESS)

type AppDb = Either Error Database

type DbRef = Ref AppDb

type AppSetupEffs e = App (console :: CONSOLE, err :: EXCEPTION, ref :: REF, ajax :: AJAX, ngrok :: Ngrok, db :: DB | e)

type AppEffs e a = Eff (err :: EXCEPTION, console :: CONSOLE, ref :: REF, ajax :: AJAX, ngrok :: Ngrok, express :: EXPRESS, db :: DB | e ) a

type AuthEffs e a = Eff (ref :: REF, console :: CONSOLE, err :: EXCEPTION, db :: DB | e) a

type HandlerAuthEffs e = Handler (ref :: REF, console :: CONSOLE, err :: EXCEPTION, db :: DB | e)

type WebHookHEffs e = Handler (ref :: REF, console :: CONSOLE, ajax :: AJAX, ngrok :: Ngrok, err :: EXCEPTION, db :: DB | e)

type JWToken = { token :: String }
