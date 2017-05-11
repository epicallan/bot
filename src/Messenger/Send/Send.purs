module Messenger.Send where

import Control.Monad.Aff (Aff, attempt, launchAff)
import Control.Monad.Aff.Console (error, info)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Foreign (F)
import Data.Foreign.Generic (decodeJSON)
import Data.Tuple (Tuple(..))
import Messenger.Types (SendEff, SendResponse, AccessToken)
import Messenger.Types.MessageEvent (Response(..))
import Messenger.Types.Send (TextMessage)
import Network.HTTP.Affjax (post, AJAX)
import Prelude (Unit, bind, void, ($), (<>), show, discard)
import Unsafe.Coerce (unsafeCoerce)
import Utils (multpleErrorsToStr)

unsafelyToRequestable :: forall a. a -> Json
unsafelyToRequestable = unsafeCoerce

sendResponse :: forall e. AccessToken -> Response -> SendEff e Unit
sendResponse token response = void $ launchAff do
  case response of
    Text (Tuple id text)  -> do
      let mRequest = { recipient: { id }, message : { text } } :: TextMessage
      callSenderAPI token mRequest
    Image (Tuple id msg) -> info "hey"
    Video (Tuple id msg) -> info "hey"
    Audio (Tuple id msg) -> info "hey"



callSenderAPI :: forall e a. AccessToken -> a -> Aff (ajax :: AJAX,  console :: CONSOLE | e) Unit
callSenderAPI token req = do
  liftEff $ log "sending json"
  eitherRes  <- attempt $ post ( fburl token) $ unsafelyToRequestable req
  case eitherRes of
    Left err      -> error $ message err
    Right payload -> do
      let eitherSendResponse = runExcept $ decodeJSON payload.response :: F SendResponse
      case eitherSendResponse of
        Left errs -> error $ "sendResponse decoding errors: " <> multpleErrorsToStr errs
        Right res -> info $ show res
  where
    fburl = (<>) "https://graph.facebook.com/v2.9/me/messages?access_token="
