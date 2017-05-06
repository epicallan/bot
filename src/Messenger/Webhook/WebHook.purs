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
import Data.Maybe (Maybe(..), maybe)
import Database.Mongo.Mongo (Database)
import Messenger.Config (FbMessengerConf, fbConf)
import Messenger.Foreign (createNgrokProxy')
import Messenger.Model.Webhook (saveWebhook, findWebhook)
import Messenger.Types (AccessTokenJson(..), FbBase, FbWebhookRequest(..), UserId, WebHookSetUpAff, WebHookSetUpEffs, AccessToken)
import Network.HTTP.Affjax (AJAX, URL, get, post)
import Prelude (bind, ($), (<>), pure, void)
import Utils (multpleErrorsToStr)

fbBase = "https://graph.facebook.com/v2.7/oauth/access_token" :: FbBase

-- | for getting access_token
fbOauthUrl ::  FbMessengerConf -> String
fbOauthUrl conf = fbBase
  <> "?client_id=" <> conf.appId
  <> "&client_secret=" <> conf.appSecret
  <> "&grant_type=client_credentials"

fbPostsubcriptionUrl :: FbMessengerConf -> AccessToken -> URL
fbPostsubcriptionUrl conf accessToken
  = fbBase <> conf.appId <> "/subscriptions?access_token=" <> accessToken

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
                  -> Aff (ajax :: AJAX,  console :: CONSOLE | e) (Maybe AccessToken)
initfbWebhook conf url = do
  eitherRes <- attempt $ get $ fbOauthUrl conf
  case eitherRes of
    Left err  -> (error $ message err) *> pure Nothing
    Right fbAuthRes -> do
      let eitherJson = runExcept $ readJSON fbAuthRes.response :: F AccessTokenJson
      case eitherJson of
        Left errors ->
          (error $ "error reading auth Json" <> multpleErrorsToStr errors) *> pure Nothing
        Right (AccessTokenJson { token }) -> do
          fbGenEither <- attempt $ post (fbPostsubcriptionUrl conf token) $ fbWebhookRequestJson url conf
          case fbGenEither of
            Left err -> (error $ message err) *> pure Nothing
            Right res -> (info res.response) *> (pure $ Just token)


setupFbWebhook :: forall e. Database -> UserId -> WebHookSetUpAff e
setupFbWebhook database userId = do
  eitherUrl <- attempt $ createNgrokProxy' 8080
  case eitherUrl of
    Left err  -> log $ message err
    Right ngrokUrl -> do
      let userWbUrl = ngrokUrl <> "webhook/" <> userId
      maybeAccessToken <- initfbWebhook fbConf userWbUrl
      maybe (error $ "No accessToken") (saveWebhook database userId userWbUrl) maybeAccessToken

main :: forall e. Database -> UserId -> WebHookSetUpEffs e
main database userId = void $ launchAff do
  maybeWebhook <- findWebhook database userId
  case maybeWebhook of
    Nothing -> setupFbWebhook database userId
    Just webhook -> info "using webhook: "
