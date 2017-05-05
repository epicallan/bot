module Messenger.Webhook where
import Control.Monad.Aff (Aff, attempt, launchAff)
import Control.Monad.Aff.Console (error, info, log)
import Control.Monad.Eff.Class (liftEff)
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
import Messenger.Config (FbMessengerConf, fbConf)
import Messenger.Foreign (createNgrokProxy')
import Messenger.Model.Webhook (saveWebhook, findWebhook)
import Messenger.Types (AccessTokenJson(..), FbBase, FbWebhookRequest(..), UserId, WebHookSetUpEffs,  WebHookSetUpAff)
import Network.HTTP.Affjax (AJAX, URL, get, post)
import Prelude (Unit, bind, show, ($), (<>), pure, unit, void)
import Utils (multpleErrorsToStr)

fbBase = "https://graph.facebook.com/v2.7/oauth/access_token" :: FbBase

-- | for getting access_token
fbOauthUrl ::  FbMessengerConf -> String
fbOauthUrl conf = fbBase
  <> "?client_id=" <> conf.appId
  <> "&client_secret=" <> conf.appSecret
  <> "&grant_type=client_credentials"

fbPostsubcriptionUrl :: FbMessengerConf -> AccessTokenJson -> URL
fbPostsubcriptionUrl conf (AccessTokenJson accessToken)
  = fbBase <> conf.appId <> "/subscriptions?access_token=" <> accessToken.token

fbWebhookRequestJson :: URL -> FbMessengerConf -> Json
fbWebhookRequestJson callbackurl conf  =
      let fields = ["message_deliveries", "message_reads", "messages", "messaging_optins",
                  "messaging_postbacks", "messaging_referrals"]
          fbWebhookRequest = FbWebhookRequest { object : "page"
                                              , verifyToken : conf.verifyToken
                                              , callbackUrl : callbackurl
                                              , fields }
  in encodeJson $ fbWebhookRequest

initfbWebhook :: forall e. FbMessengerConf -> URL
                  -> Aff (ajax :: AJAX,  console :: CONSOLE | e) Unit
initfbWebhook conf url = do
  eitherRes <- attempt $ get $ fbOauthUrl conf
  case eitherRes of
    Left err  -> error $ message err -- TODO better error handling or logging
    Right fbAuthRes -> do
      let eitherJson = runExcept $ readJSON fbAuthRes.response :: F AccessTokenJson
      case eitherJson of
        Left errors  -> error $ "error reading auth Json" <> multpleErrorsToStr errors
        Right tokenJson -> do
          fbGenEither <- attempt $ post (fbPostsubcriptionUrl conf tokenJson) $ fbWebhookRequestJson url conf
          case fbGenEither of
            Left err -> error $ message err
            Right res -> info res.response



setupFbWebhook :: forall e. Database -> UserId -> WebHookSetUpAff e
setupFbWebhook database userId = do
  eitherUrl <- attempt $ createNgrokProxy' 8080
  case eitherUrl of
    Left err  -> log $ message err
    Right url -> do
      log url
      initfbWebhook fbConf url
      saveWebhook database userId url

main :: forall e. Database -> UserId -> WebHookSetUpEffs e
main database userId = void $ launchAff do
  maybeWebhook <- findWebhook database userId
  case maybeWebhook of
    Nothing -> do
      setupFbWebhook database userId
      pure unit
    Just webhook -> do
      info "using webhook: "
      pure unit
