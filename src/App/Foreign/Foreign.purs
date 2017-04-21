module App.Foreign where

import App.Config.Config (GoogleStrategy, JWTSecret)
import App.Model.User (User)
import App.Types (JWToken, DbRef, AuthEffs)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn3)
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (ExpressM, Request, Response)
import Prelude (Unit, ($))
foreign import data PASSPORT :: !

foreign import jsonBodyParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import morgan :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import cookieParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)


foreign import passportInitialize :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import googleAuthStrategy :: forall e. GoogleStrategy
                                    -> Eff (passport:: PASSPORT, console :: CONSOLE | e) Unit

foreign import _googleAuthReturn :: forall e. (Foreign ->  AuthEffs e JWToken)
                                  -> Request -> Response -> (ExpressM e Unit) -> (ExpressM e Unit)

foreign import _googleAuth :: forall e. Request -> Response -> (ExpressM e Unit) -> (ExpressM e Unit)

googleAuthReturn :: forall e. (DbRef -> Foreign -> AuthEffs e JWToken) -> DbRef -> Handler e
googleAuthReturn authHandler dbRef =
  HandlerM \req resp next -> liftEff $ _googleAuthReturn (\userF -> authHandler dbRef userF) req resp next

googleAuth :: forall e. Handler e
googleAuth = HandlerM \req resp next -> liftEff $ _googleAuth req resp next

foreign import createJwtToken :: JWTSecret -> User -> String
