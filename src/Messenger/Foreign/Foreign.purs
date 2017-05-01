module Messenger.Foreign where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (runFn2, Fn2)
import Data.URI (Port)
import Prelude (Unit)

foreign import data Ngrok :: !


foreign import _createNgrokProxy :: forall e. Fn2
                                (String -> Eff (ngrok :: Ngrok | e) Unit)
                                Port
                                (Eff (ngrok :: Ngrok | e) Unit)

-- only used in development
createNgrokProxy' :: forall e. Port -> Aff (ngrok :: Ngrok | e) String
createNgrokProxy' port = makeAff (\error success -> runFn2 _createNgrokProxy success port)
