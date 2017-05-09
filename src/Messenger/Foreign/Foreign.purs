module Messenger.Foreign where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (runFn2, Fn2)
import Data.URI (Port)
import Messenger.Types(Ngrok)
import Prelude (Unit)



-- only used in development. Creates an https domain that proxies requests to our app
foreign import _startNgrok :: forall e. Fn2
                                (String -> Eff (ngrok :: Ngrok | e) Unit)
                                Port
                                (Eff (ngrok :: Ngrok | e) Unit)
startNgrok :: forall e. Port -> Aff (ngrok :: Ngrok | e) String
startNgrok port = makeAff (\error success -> runFn2 _startNgrok success port)

foreign import stopNgrok :: forall e. Eff (ngrok :: Ngrok | e) Unit
