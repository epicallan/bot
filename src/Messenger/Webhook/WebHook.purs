module Messenger.Webhook where
import Control.Monad.Aff (Aff, attempt, launchAff)
import Control.Monad.Aff.Console (error, info, log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Foreign (F)
import Data.Foreign.Generic (decodeJSON)
import Messenger.Config (fbConf)
import Messenger.Foreign (startNgrok)
import Messenger.Types (FbMessengerConf, AccessToken, AccessTokenJson(..), FbBase
  , FbWebhookRequest(..), UserId, WebHookSetUpAff, WebHookSetUpEffs, SubcribeAff)
import Network.HTTP.Affjax (AJAX, URL, delete, get, post)
import Prelude (Unit, bind, show, unit, void, ($), (<>), discard)
import Utils (multpleErrorsToStr)

fbBase = "https://graph.facebook.com" :: FbBase

-- | for getting access_token
fbOauthUrl ::  FbMessengerConf -> String
fbOauthUrl conf = fbBase <> "/v2.8/oauth/access_token"
  <> "?client_id=" <> conf.appId
  <> "&client_secret=" <> conf.appSecret
  <> "&grant_type=client_credentials"

fbPostsubcriptionUrl :: FbMessengerConf -> AccessToken -> URL
fbPostsubcriptionUrl conf accessToken
  = fbBase <> "/" <> conf.appId <> "/subscriptions?access_token=" <> accessToken

fbWebhookRequestJson :: URL -> FbMessengerConf -> Json
fbWebhookRequestJson callbackurl conf  =
      let fields = ["message_deliveries", "message_reads", "messages", "messaging_optins",
                  "messaging_postbacks", "messaging_referrals"]
          fbWebhookRequest = FbWebhookRequest { object : "page"
                                              , verify_token : conf.verifyToken
                                              , callback_url : callbackurl
                                              , fields }
  in encodeJson $ fbWebhookRequest

initfbWebhook :: forall e. FbMessengerConf -> URL
                  -> Aff (ajax :: AJAX,  console :: CONSOLE | e) Unit
initfbWebhook conf url = do
  eitherRes <- attempt $ get $ fbOauthUrl conf
  case eitherRes of
    Left err  -> error $ "fboauth access: " <> message err
    Right fbAuthRes -> do
      let eitherJson = runExcept $ decodeJSON fbAuthRes.response :: F AccessTokenJson
      info $ "fb response: " <> fbAuthRes.response
      case eitherJson of
        Left errors ->
          error $ "error reading auth Json: " <> multpleErrorsToStr errors
        Right (AccessTokenJson { access_token }) -> do
          let payload = fbWebhookRequestJson url conf
          info $ " payload: \n " <> (show $ encodeJson payload)
          subEither <- attempt $ post (fbPostsubcriptionUrl conf access_token) $ payload
          case subEither of
            Left err  -> error $ message err
            Right res -> info  $ "added webhook: " <> res.response


setupFbWebhook :: forall e. UserId -> WebHookSetUpAff e -- TODO UserId maybe useless
setupFbWebhook userId = do
  eitherUrl <- attempt $ startNgrok 8080
  case eitherUrl of
    Left err  -> log $ message err
    Right ngrokUrl -> do
      let userWbUrl = ngrokUrl <> "/webhook/" <> userId
      initfbWebhook fbConf userWbUrl

subscribePage :: forall e.  SubcribeAff e
subscribePage = do
  let url = fbBase <> "/v2.8/me/subscribed_apps?access_token=" <> fbConf.accessToken
  subEither <- attempt $ delete url
  case subEither of
    Left err  -> error $ message err
    Right res -> info  $ "subscribed webhook: " <> res.response

unSubscribePage :: forall e. SubcribeAff e
unSubscribePage = do
  let url = fbBase <> "/v2.8/me/subscribed_apps?access_token=" <> fbConf.accessToken
  subEither <- attempt $ post url unit
  case subEither of
    Left err  -> error $ message err
    Right res -> info  $ "unsubscribed webhook: " <> res.response


main :: forall e. UserId -> WebHookSetUpEffs e
main userId = void $ launchAff do
  setupFbWebhook userId
  subscribePage
