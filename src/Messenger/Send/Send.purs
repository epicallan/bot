module Messenger.Send.Send where

import Control.Monad.Aff (Aff, attempt, launchAff)
import Control.Monad.Aff.Console (error, info)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, message)
import Control.Monad.Except (runExcept)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Data.Tuple (Tuple(..))
import Messenger.Model.MessageEvent (Response(..))
import Messenger.Model.Send (TextMessage)
import Messenger.Types (SendResponse, SendEff)
import Network.HTTP.Affjax (post, AJAX)
import Prelude (Unit, bind, void, ($), (<>), show)
import Unsafe.Coerce (unsafeCoerce)
import Utils (multpleErrorsToStr)

unsafelyToRequestable :: forall a. a -> Json
unsafelyToRequestable = unsafeCoerce

sendResponse :: forall e. Response -> SendEff e Unit
sendResponse response = void $ launchAff do
  case response of
    Text (Tuple id text)  -> do
      let mRequest = { recipient: { id }, message : { text } } :: TextMessage
      callSenderAPI mRequest
    Image (Tuple id msg) -> info "hey"
    Video (Tuple id msg) -> info "hey"
    Audio (Tuple id msg) -> info "hey"



callSenderAPI :: forall e a. a -> Aff (ajax :: AJAX,  console :: CONSOLE | e) Unit
callSenderAPI req = do
  liftEff $ log "sending json"
  eitherRes  <- attempt $ post "url" (unsafelyToRequestable req)
  case eitherRes of
    Left err      -> error $ message err
    Right payload -> do
      let eitherSendResponse = runExcept $ readJSON payload.response :: F SendResponse
      case eitherSendResponse of
        Left errs -> error $ "sendResponse decoding errors: " <> multpleErrorsToStr errs
        Right res -> info $ show res
