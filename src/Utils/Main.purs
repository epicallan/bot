module Utils where

import Data.Foreign (MultipleErrors, renderForeignError)
import Data.List.Types (toList)
import Data.Foldable (foldl)
import Prelude ((<>), ($))

multpleErrorsToStr :: MultipleErrors -> String -- lets change to traverse_
multpleErrorsToStr x = foldl (\acc cur -> renderForeignError cur <> acc) "" $ toList x
