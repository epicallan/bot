module Messenger.Webhook where
import Control.Monad ((*>))
import Control.Monad.Aff (Aff, attempt, launchAff)
import Control.Monad.Aff.Console (error, info, log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Except (runExcept)
import Data.Argonaut (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Data.Maybe (Maybe(..))
import Database.Mongo.Mongo (Database)
import Messenger.Config (fbConf)
import Messenger.Foreign (createNgrokProxy')
import Messenger.Model.Webhook (saveWebhook, findWebhook)
import Messenger.Types (FbMessengerConf, AccessToken, AccessTokenJson(..), FbBase, FbWebhookRequest(..), UserId, WebHookSetUpAff, WebHookSetUpEffs, Webhook)
import Network.HTTP.Affjax (AJAX, URL, get, post)
import Prelude (bind, show, void, ($), (<>), Unit)
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
      let eitherJson = runExcept $ readJSON fbAuthRes.response :: F AccessTokenJson
      info $ "fb response: " <> fbAuthRes.response
      case eitherJson of
        Left errors ->
          error $ "error reading auth Json: " <> multpleErrorsToStr errors
        Right (AccessTokenJson { access_token }) -> do
          let payload = fbWebhookRequestJson url conf
          info $ " payload: \n " <> (show $ encodeJson payload)
          fbGenEither <- attempt $ post (fbPostsubcriptionUrl conf access_token) $ payload
          case fbGenEither of
            Left err  -> error $ message err
            Right res -> info  $ "added webhook: " <> res.response


setupFbWebhook :: forall e. Database -> UserId -> WebHookSetUpAff e
setupFbWebhook database userId = do
  eitherUrl <- attempt $ createNgrokProxy' 8080
  case eitherUrl of
    Left err  -> log $ message err
    Right ngrokUrl -> do
      let userWbUrl = ngrokUrl <> "/webhook/" <> userId
      initfbWebhook fbConf userWbUrl *> saveWebhook database userId userWbUrl -- TODO we may not need to save the webhook url afterall

main :: forall e. Database -> UserId -> WebHookSetUpEffs e
main database userId = void $ launchAff do
  (maybeWebhook :: Maybe Webhook) <- findWebhook database userId
  case maybeWebhook of
    Nothing -> setupFbWebhook database userId
    Just wb -> info $ "Already using webhook: " <> (show $ encodeJson wb)
