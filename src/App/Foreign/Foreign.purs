module App.Foreign where

import App.Types (JWToken, DbRef, AuthEffs, HandlerAuthEffs, GoogleStrategy, JWTSecret, User, PASSPORT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn3, runFn3, runFn4, Fn4)
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (ExpressM, Request, Response)
import Prelude (Unit, ($))


foreign import jsonBodyParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import morgan :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import passportInitialize :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import googleAuthStrategy :: forall e. GoogleStrategy
                                    -> Eff (passport:: PASSPORT, console :: CONSOLE | e) Unit

foreign import _googleAuthReturn :: forall e. (Foreign -> Eff e JWToken)
                                  -> Request -> Response -> (ExpressM e Unit) -> (ExpressM e Unit)

foreign import _protectedRoutesHandler ::  forall e. Fn4 JWTSecret Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import _googleAuth :: forall e. Request -> Response -> (ExpressM e Unit) -> (ExpressM e Unit)

googleAuthReturn :: forall e. (DbRef -> Foreign -> AuthEffs e JWToken) -> DbRef -> HandlerAuthEffs e
googleAuthReturn cb dbRef =
  HandlerM \req resp next -> liftEff $ _googleAuthReturn (cb dbRef) req resp next

googleAuth :: forall e. Handler e
googleAuth = HandlerM \req resp next -> liftEff $ _googleAuth req resp next

protectedRoutesHandler :: forall e. JWTSecret -> Handler e
protectedRoutesHandler secret = HandlerM \req resp next ->
    liftEff $ runFn4 _protectedRoutesHandler secret req resp next

foreign import createJwtToken :: JWTSecret -> User -> String

foreign import _setUserJwData :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

setUserJwData :: forall e. Handler e
setUserJwData = HandlerM \req res next ->
    liftEff $ runFn3 _setUserJwData req res next
