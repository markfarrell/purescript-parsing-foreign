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

null :: Foreign
null = from "null"

keys :: Effect Unit
keys = do
  _ <- output []         $ to {}
  _ <- output ["a"]      $ to { a : null }
  _ <- output ["a", "b"] $ to { a : null, b : null }
  _ <- output []         $ to []
  _ <- output ["0"]      $ to [ null ]
  _ <- output ["0", "1"] $ to [ null, null ]
  _ <- output []         $ null
  _ <- output []         $ to "a"
  _ <- output []         $ to 0.0
  _ <- output []         $ to 1.0
  _ <- output []         $ to 1
  pure unit
  where
    keys'   = F.keys V.input
    output  = \expect check -> E.output expect check keys'

index :: Effect Unit
index = do
  _ <- success "a" $ to { a : null }
  _ <- success "a" $ to { a : null, b : null}
  _ <- success "b" $ to { a : null, b : null}
  _ <- success "0" $ to [null]
  _ <- success "1" $ to [null,null]
  _ <- success "1" $ to [null]
  _ <- success "a" $ to [null]
  pure unit
  where
    index'  = flip F.index V.input
    success = \x check -> E.success check (index' x)

string :: Effect Unit
string = do
  _ <- success "a" $ to { a : "a" }
  _ <- failure "a" $ to { a : null }
  pure unit
  where
    string'  = flip F.string V.input
    success = \x check -> E.success check (string' x)
    failure = \x check -> E.failure check (string' x)

boolean :: Effect Unit
boolean = do
  _ <- success "a" $ to { a : true }
  _ <- failure "a" $ to { a : null }
  pure unit
  where
    boolean'  = flip F.boolean V.input
    success = \x check -> E.success check (boolean' x)
    failure = \x check -> E.failure check (boolean' x)

number :: Effect Unit
number = do
  _ <- success "a" $ to { a : 0.0 }
  _ <- failure "a" $ to { a : null }
  pure unit
  where
    number'  = flip F.number V.input
    success = \x check -> E.success check (number' x)
    failure = \x check -> E.failure check (number' x)

int :: Effect Unit
int = do
  _ <- success "a" $ to { a : 0 }
  _ <- failure "a" $ to { a : 0.1 }
  _ <- failure "a" $ to { a : null }
  pure unit
  where
    int'  = flip F.int V.input
    success = \x check -> E.success check (int' x)
    failure = \x check -> E.failure check (int' x)

array :: Effect Unit
array = do
  _ <- success "a" $ to { a : [] }
  _ <- success "a" $ to { a : [null] }
  _ <- failure "a" $ to { a : {} }
  _ <- failure "a" $ to { a : null }
  pure unit
  where
    array'  = flip F.array V.input
    success = \x check -> E.success check (array' x)
    failure = \x check -> E.failure check (array' x)

nullOrUndefined :: Effect Unit
nullOrUndefined = do
  _ <- success "a" $ to { a : 0.0 }
  _ <- success "a" $ to { a : "a" }
  _ <- success "a" $ to { a : [] }
  _ <- success "a" $ to { a : {} }
  _ <- success "a" $ to { a : null }
  pure unit
  where
    nullOrUndefined'  = flip F.nullOrUndefined V.input
    success = \x check -> E.success check (nullOrUndefined' x)
    failure = \x check -> E.failure check (nullOrUndefined' x)

main :: Effect Unit
main = do
  _ <- keys
  _ <- index
  _ <- string
  _ <- number
  _ <- int
  _ <- array
  _ <- nullOrUndefined
  pure unit
