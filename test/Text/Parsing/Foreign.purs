module Test.Text.Parsing.Foreign
 ( main
 ) where

import Prelude

import Data.Either (Either(..))

import Effect (Effect)

import Foreign (Foreign)

import Unsafe.Coerce (unsafeCoerce)

import FFI.Foreign.JSON as J

import Text.Parsing.Expect as E
import Text.Parsing.Foreign as F
import Text.Parsing.Combinators.Validation as V

to :: forall a. a -> Foreign
to = unsafeCoerce

from :: String -> Foreign
from x = case J.parse x of
  (Left _)  -> to {}
  (Right y) -> y

keys :: Effect Unit
keys = do
  _ <- success $ to {}
  _ <- success $ to { a : null }
  _ <- success $ to { a : null, b : null }
  _ <- success $ to []
  _ <- success $ to [ null ]
  _ <- success $ to [ null, null ]
  _ <- failure null
  _ <- failure $ to "a"
  _ <- failure $ to 0.0
  _ <- failure $ to 1.0
  _ <- failure $ to 1
  pure unit
  where
    null    = from "null"
    keys' = F.keys V.input
    success = \check -> E.success check keys'
    failure = \check -> E.failure check keys'

main :: Effect Unit
main = do
  _ <- keys
  pure unit
