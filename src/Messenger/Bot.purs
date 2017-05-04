module Messenger.Bot where
import Control.Bind ((>>=))
import Control.Monad.Eff (foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error, info, log)
import Control.Monad.Except (runExcept)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Messenger.Model.MessageEvent (EventAction(..), MessageEffs, MessageEntry(..), MessageEvent(..), Messaging(..), Postback(..), Response(..), WebHookEffs)
import Node.Express.Request (getBody)
import Node.Express.Response (setStatus)
import Prelude (Unit, bind, const, join, map, pure, void, ($), (<>))
import Utils (multpleErrorsToStr)


sendResponse :: forall e. (Maybe Response) -> WebHookEffs e
sendResponse response = case response of
  Nothing -> setStatus 200
  Just (Text (Tuple id msg))  -> setStatus 200
  Just (Image (Tuple id msg)) -> setStatus 200
  Just (Audio (Tuple id msg)) -> setStatus 200
  Just (Video (Tuple id msg)) -> setStatus 200



getEventAction :: Messaging -> Maybe EventAction
getEventAction (Messaging ms) =
  case unNullOrUndefined ms.postback of
    Just postback -> Just (EventP postback)
    Nothing -> do
      case unNullOrUndefined ms.message of
        Just message -> Just (EventM message)
        Nothing -> do
          case unNullOrUndefined ms.read of
            Just read -> Just (EventR read)
            Nothing -> Nothing


runTest :: forall e. EventAction -> MessageEffs e (Maybe Response)
runTest msg = do
  liftEff $ log "hey  there"
  pure $ Just (Text (Tuple "" ""))

runResponse :: forall e. (EventAction -> MessageEffs e (Maybe Response))
                -> EventAction
                -> WebHookEffs e
runResponse h e = do
  mR <- liftEff $ runTest e
  sendResponse mR

messageEventRunner :: forall e. (EventAction -> MessageEffs e (Maybe Response)) -> MessageEvent -> WebHookEffs e
messageEventRunner handler (MessageEvent messageEvent@{ object, entry}) = do
  let eventActions = join $ map (\(MessageEntry messageEntry@{ messaging }) -> map getEventAction messaging) entry
  case (head eventActions) of
    Nothing -> setStatus 200
    Just maybeAction -> do
      case maybeAction of
        Nothing -> setStatus 200
        Just action -> do
          liftEff $ log "hell"
          runResponse handler action
      -- liftEff $ maybe (const $ setStatus 200) (c) $ maybeAction
      -- setStatus 200
  --
  -- eventAction <- head eventActions
  -- case eventAction of
  --   Nothing -> setStatus 200
  --   Just _ -> setStatus 200
      --
      -- setStatus 200


      -- where traverseMesssaging msg = runMsgHandler handler msg


webhookMessagesHandler :: forall e. (EventAction -> MessageEffs e (Maybe Response)) -> WebHookEffs e
webhookMessagesHandler handler = do
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
        Right messageEvent -> messageEventRunner handler messageEvent
