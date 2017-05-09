module Utils where

import Data.Foreign (MultipleErrors, renderForeignError)
import Data.List.Types (toList)
import Data.Foldable (foldl)
import Prelude (($), (<>))


multpleErrorsToStr :: MultipleErrors -> String
multpleErrorsToStr x =
  foldl (\acc cur -> renderForeignError cur <> "\n" <> acc) "" $ toList x
