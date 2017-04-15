module App.Foreign where

-- import App.Config.Config (FacebookStrategy)
import App.Model.User (User)
-- import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn3)
import Node.Express.Types (ExpressM, Request, Response)
import Prelude (Unit)

foreign import data PASSPORT :: *
foreign import data MONGO :: *

foreign import jsonBodyParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import cookieParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import expressSession :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import passportSession :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import passportInitialize :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import facebookAuth :: forall a. a -> (User -> MONGO) -> PASSPORT
