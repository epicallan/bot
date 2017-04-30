module Bot.MessageEvent where

import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (defaultOptions, readGeneric)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Prelude (class Show)

newtype Referral = Referral
  { ref :: String
  , source :: String
  , type :: String
  }

newtype Coordinates = Coordinates
 { lat :: Int
 , long :: Int
 }

newtype MessageAttachementPayload = MessageAttachementPayload
  { url :: NullOrUndefined String
  , coordinates :: NullOrUndefined  Coordinates
  }

newtype Postback = Postback
  { payload :: String
  , referral :: NullOrUndefined Referral
  }

newtype MessageAttachement = MessageAttachement
  { type :: String
  , payload ::  MessageAttachementPayload
 }

newtype QuickReply = QuickReply { payload :: String }

newtype Message = Message
  { mid :: String
  , text :: NullOrUndefined String
  , quick_reply :: NullOrUndefined QuickReply
  , attachments :: NullOrUndefined (Array MessageAttachement)
  }

newtype Read = Read
  { watermark :: Int
  , seq :: Int
  }

newtype SenderRecipientId = SenderRecipientId { id :: String }

newtype Messaging = Messaging
  { sender :: SenderRecipientId
  , recipient :: SenderRecipientId
  , timestamp :: Int
  , postback :: NullOrUndefined Postback
  , message :: NullOrUndefined Message
  , read :: NullOrUndefined Read
  }

newtype MessageEntry = MessageEntry
  { id :: String
  , time :: Int
  , messaging :: Array Messaging
  }

newtype MessageEvent = MessageEvent
  { object :: String
  , entry :: Array  MessageEntry
  }

derive instance genericQuickReply :: Generic QuickReply _
instance showQuickReply :: Show QuickReply where show = genericShow
instance isForeignQuickReply :: IsForeign QuickReply where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

derive instance genericCoordinates :: Generic Coordinates _
instance showCoordinates :: Show Coordinates where show = genericShow
instance isForeignCoordinates :: IsForeign Coordinates where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

derive instance genericMessageAttachementPayload :: Generic  MessageAttachementPayload _
instance showMessageAttachementPayload :: Show  MessageAttachementPayload where show = genericShow
instance isForeignMessageAttachementPayload :: IsForeign  MessageAttachementPayload where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

derive instance genericReferral :: Generic Referral _
instance showReferral :: Show Referral where show = genericShow
instance isForeignReferral :: IsForeign Referral where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

derive instance genericMessageAttachement :: Generic MessageAttachement _
instance showMessageAttachement :: Show MessageAttachement where show = genericShow
instance isForeignMessageAttachement :: IsForeign MessageAttachement where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

derive instance genericRead :: Generic Read _
instance showRead :: Show Read where show = genericShow
instance isForeignRead :: IsForeign Read where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

derive instance genericMessage :: Generic Message _
instance showMessage :: Show Message where show = genericShow
instance isForeignMessage :: IsForeign Message where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

derive instance genericPostback :: Generic Postback _
instance showPostback :: Show Postback where show = genericShow
instance isForeignPostback :: IsForeign Postback where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

derive instance genericSenderRecipientId :: Generic SenderRecipientId _
instance showSenderRecipientId :: Show SenderRecipientId where show = genericShow
instance isForeignSenderRecipientId :: IsForeign SenderRecipientId where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

derive instance genericMessaging :: Generic Messaging _
instance showMessaging :: Show Messaging where show = genericShow
instance isForeignMessaging :: IsForeign Messaging where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

derive instance genericMessageEntry :: Generic MessageEntry _
instance showMessageEntry :: Show MessageEntry where show = genericShow
instance isForeignMessageEntry :: IsForeign MessageEntry where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

derive instance genericMessageEvent :: Generic MessageEvent _
instance showMessageEvent :: Show MessageEvent where show = genericShow
instance isForeignMessageEvent :: IsForeign MessageEvent where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x
