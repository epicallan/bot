module Messenger.Types.Send where

import Messenger.Types (UserId)

type TextMessage =
  { recipient :: { id :: UserId }
  , message ::   { text :: String }
  }
