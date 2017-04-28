module Bot.Foreign where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Function.Uncurried (runFn3, Fn3)
import Data.URI (Port)
import Prelude (Unit)

foreign import data Ngrok :: !


foreign import _createNgrokProxy :: forall e. Fn3
                                (Error  -> Eff (ngrok :: Ngrok | e) Unit)
                                (String -> Eff (ngrok :: Ngrok | e) Unit)
                                Port
                                (Eff (ngrok :: Ngrok | e) Unit)

-- only used in development
createNgrokProxy' :: forall e. Port -> Aff (ngrok :: Ngrok | e) String
createNgrokProxy' port = makeAff (\error success -> runFn3 _createNgrokProxy error success port)
