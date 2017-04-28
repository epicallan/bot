module Bot.Webhook where

import App.Model.User (UserId)
import Bot.Config (FbMessengerConf)
import Bot.Foreign (createNgrokProxy', Ngrok)
import Bot.Types (AccessTokenJson(..), FbBase, FbWebHookRequest(..), Url)
import Control.Monad (void)
import Control.Monad.Aff (attempt, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, message)
import Control.Monad.Except (runExcept)
import Data.Argonaut (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Database.Mongo.Mongo (DB)
import Network.HTTP.Affjax (AJAX, get, post)
import Prelude (Unit, ($), (<>), bind)

fbBase = "https://graph.facebook.com/v2.7/oauth/access_token" :: FbBase

-- | for getting access_token
fbOauthUrl ::  FbMessengerConf -> String
fbOauthUrl conf = fbBase
  <> "?client_id=" <> conf.appId
  <> "&client_secret=" <> conf.appSecret
  <> "&grant_type=client_credentials"

fbPostsubcriptionUrl :: FbMessengerConf -> AccessTokenJson -> String
fbPostsubcriptionUrl conf (AccessTokenJson accessToken)
  = fbBase <> conf.appId <> "/subscriptions?access_token=" <> accessToken.access_token

fbWebHookRequestJson :: Url -> FbMessengerConf -> Json
fbWebHookRequestJson callbackurl conf  =
      let fields = ["message_deliveries", "message_reads", "messages", "messaging_optins",
                  "messaging_postbacks", "messaging_referrals"]
          fbWebHookRequest = FbWebHookRequest { object : "page"
                                              , verifyToken : conf.verifyToken
                                              , callbackUrl : callbackurl
                                              , fields }
  in encodeJson $ fbWebHookRequest

setUpNewFbWebHook :: forall e. Url -> FbMessengerConf -> Eff (ajax :: AJAX, err:: EXCEPTION, console :: CONSOLE | e) Unit
setUpNewFbWebHook url conf =  void $ launchAff do
  eitherRes <- attempt $ get $ fbOauthUrl conf
  case eitherRes of
    Left err  -> liftEff $ log $ message err -- TODO better error handling or logging
    Right fbres ->  do
      let eitherJson = runExcept $ readJSON fbres.response :: F AccessTokenJson
      case eitherJson of
        Left _ -> liftEff $ log "error reading Json"
        Right tokenJson -> do
          fbGenEither <- attempt $ post "/kda" "allena"
          case fbGenEither of
            Left err -> liftEff $ log $ message err
            Right res -> liftEff $ log res.response
          -- let fbGenEither r = runExcept $ readJSON r.response :: F FbGenericResponse
          -- let decodedResEither = either (Left) fbGenEither fbGenericRes
          -- case decodedResEither of
          --   Left _ -> liftEff $ log "decoding error"
          --   Right _ -> liftEff $ log "set up weebhook"

createFbWebHook :: forall e. UserId
                -> FbMessengerConf
                -> Eff (ngrok:: Ngrok, ajax :: AJAX, err:: EXCEPTION, console :: CONSOLE | e) Unit
createFbWebHook userId fbConf= void $ launchAff do
  eitherUrl <- attempt $ createNgrokProxy' 8080
  case eitherUrl of
    Left err  -> liftEff $ log $ message err
    Right url -> do
      liftEff $ log url
      liftEff $ setUpNewFbWebHook url fbConf
