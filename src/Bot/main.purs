module Bot.Main where
import Bot.Model.MessageEvent (MessageEvent, MessageEventHandler, MessageEventRunner, MessageEffs)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Node.Express.Request (getBody)
import Node.Express.Response (setStatus)
import Prelude (bind, (<>), ($))
import Utils (multpleErrorsToStr)

webhookMessagesHandler :: forall e. MessageEventRunner e -> MessageEventHandler e -> MessageEffs e
webhookMessagesHandler runEvent handler = do
  eitherBodyRaw <- getBody
  case eitherBodyRaw of
    Left multipleBodyErrors -> do
      liftEff $ error $ multpleErrorsToStr multipleBodyErrors
      setStatus(200)
    Right body -> do
      let eitherMessageEvent = runExcept $ readJSON body :: F MessageEvent
      case eitherMessageEvent of
        Left multipleJsonErrors -> do
          liftEff $ error $ "failed to purse json: " <> (multpleErrorsToStr multipleJsonErrors)
          setStatus(200)
        Right messageEvent -> runEvent handler messageEvent
