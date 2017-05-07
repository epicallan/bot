module App.Handler.User where
import Messenger.Webhook as Wb
import App.Config.Config (jwtSecret)
import App.Foreign (createJwtToken)
import App.Model.User (User, createUser)
import App.Types (JWToken, AuthEffs, DbRef, AddWebHookEffs)
import Control.Monad ((*>))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Ref (readRef)
import Control.Monad.Except (runExcept)
import Data.Argonaut (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, readString, unsafeFromForeign)
import Data.Maybe (Maybe(..))
import Node.Express.Handler (Handler)
import Node.Express.Request (getUserData)
import Node.Express.Response (send, setStatus)
import Prelude (bind, pure, ($), (<>))

unsafelyToJson :: Foreign -> Json
unsafelyToJson = unsafeFromForeign

parseUserRes :: Foreign -> Either String User
parseUserRes userRes = decodeJson $ unsafelyToJson userRes

-- this could be turned into a maybe but I think it will end up requiring interfacing
-- with the returned Maybe value in the js foreign file
jwtoken = { token : ""} :: JWToken

authHandler :: forall e. DbRef -> Foreign -> AuthEffs e JWToken
authHandler dbRef userPayload = do
    eitherDb <- liftEff $ readRef dbRef
    case eitherDb of
      Left err ->
        (liftEff $ log $ "Error connecting to the database" <> message err) *> pure jwtoken
      Right db -> do
        let eitherUser = parseUserRes userPayload
        case eitherUser of
          Left err ->
            (liftEff $ log $ "User parse error: " <> err) *> pure jwtoken
          Right user ->
            (liftEff $ createUser db user) *> pure { token : createJwtToken jwtSecret user }


loginHandler :: forall e. Handler e
loginHandler = send "Please go and login" -- TODO redirect to login page on front end app

indexHandler :: forall e. Handler e
indexHandler = send "You have been signed up" -- TODO redirect to login page on front end app

-- protectedHandler :: forall e. Handler e
-- protectedHandler = do
--   maybeId <- getUserData "id"
--   case maybeId of
--     Nothing -> send "No user Id redirect to login page"
--     Just foreignId ->
--       let eitherId = runExcept(readString foreignId :: F String)
--       in case eitherId of
--           Left _ -> send "Error reading authentication ID"
--           Right id -> send $ "authenticated with" <> id


addFbWebhook :: forall e. DbRef -> AddWebHookEffs e
addFbWebhook dbRef = do
  eitherDb <- liftEff $ readRef dbRef
  case eitherDb of
    Left err -> do
      liftEff $ log $ "Error connecting to the database " <> message err
      setStatus 500
    Right db -> do
      maybeId <- getUserData "id"
      case maybeId of
        Nothing -> send "No user Id redirect to login page"
        Just foreignId ->
          let eitherId = runExcept(readString foreignId :: F String)
          in case eitherId of
              Left _ -> send "Error reading authentication ID"
              Right id -> (liftEff $ Wb.main db id) *> (send "set up webhook")
