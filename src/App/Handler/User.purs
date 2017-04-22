module App.Handler.User where
import App.Config.Config (jwtSecret)
import App.Model.User (User, addUser)
import App.Foreign(createJwtToken)
import App.Types (JWToken, AuthEffs, DbRef)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Ref (readRef)
import Data.Argonaut (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..))
import Data.Foreign (Foreign, unsafeFromForeign)
import Node.Express.Handler (Handler)
import Node.Express.Response (send)
import Prelude (($), pure, bind, (<>))


unsafelyToJson :: Foreign -> Json
unsafelyToJson = unsafeFromForeign

parseUserRes :: Foreign -> Either String User
parseUserRes userRes = decodeJson $ unsafelyToJson userRes


-- TODO change type signature so that we can throw error to be caught by a error handdler
-- middleware
jwtoken = { token : "null"} :: JWToken

authHandler :: forall e. DbRef -> Foreign -> AuthEffs e JWToken
authHandler dbRef userPayload = do
    liftEff $ log $ "In authHandler"
    eitherDb <- liftEff $ readRef dbRef
    case eitherDb of
      Left err -> do
        liftEff $ log $ "Error connecting to the database for authentication " <> message err
        pure jwtoken
      Right db -> do
        let eitherUser = parseUserRes userPayload
        liftEff $ log $ "decoded user"
        case eitherUser of
          Left err -> do
            liftEff $ log $ "User parse error" <> err
            pure jwtoken
          Right user -> do
            liftEff $ log $ "adding user to db"
            addUser db user
            pure { token : createJwtToken jwtSecret user }

loginHandler :: forall e. Handler e
loginHandler = send "Please go and login" -- TODO redirect to login page on front end app



indexHandler :: forall e. Handler e
indexHandler = send "You have been signed up" -- TODO redirect to login page on front end app
