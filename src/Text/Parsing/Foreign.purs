module Text.Parsing.Foreign
  ( string
  , int
  , boolean
  , number
  , array
  , null
  , undefined
  , nullOrUndefined
  , keys
  ) where

import Prelude

import Control.Monad.State.Class (gets)
import Control.Monad.Trans.Class (lift)

import Data.Identity (Identity(..))
import Data.Maybe (Maybe)

import Data.Traversable (class Traversable)
import Data.Traversable as T

import Foreign (Foreign)
import Foreign.Index (class Index, (!))

import Foreign as F
import Foreign.Keys as K

import Text.Parsing.Parser (ParserT, Parser)
import Text.Parsing.Parser as P

import Text.Parsing.Combinators.Validation as V

input :: forall a m. Monad m => ParserT a m a
input = gets \(P.ParseState x _ _ ) -> x

hoist :: forall a m b. Monad m => Parser a b -> ParserT a m b
hoist =  P.hoistParserT (\(Identity x) -> pure x)

readString :: forall a m. Index a => Monad m => a -> ParserT Foreign m String
readString k = hoist $ do
  x <- input
  y <- V.success (x ! k >>= F.readString)
  pure y

readInt :: forall a m. Index a => Monad m => a -> ParserT Foreign m Int
readInt k = hoist $ do
  x <- input
  y <- V.success (x ! k >>= F.readInt)
  pure y

readBoolean :: forall a m. Index a => Monad m => a -> ParserT Foreign m Boolean
readBoolean k = hoist $ do
  x <- input
  y <- V.success (x ! k >>= F.readBoolean)
  pure y

readNumber :: forall a m. Index a => Monad m => a -> ParserT Foreign m Number
readNumber k = hoist $ do
  x <- input
  y <- V.success (x ! k >>= F.readNumber)
  pure y

readArray :: forall a m. Index a => Monad m => a -> ParserT Foreign m (Array Foreign)
readArray k = hoist $ do
  x <- input
  y <- V.success (x ! k >>= F.readArray)
  pure y

readNull :: forall a m. Index a => Monad m => a -> ParserT Foreign m (Maybe Foreign)
readNull k = hoist $ do
  x <- input
  y <- V.success (x ! k >>= F.readNull)
  pure y

readUndefined :: forall a m. Index a => Monad m => a -> ParserT Foreign m (Maybe Foreign)
readUndefined k = hoist $ do
  x <- input
  y <- V.success (x ! k >>= F.readUndefined)
  pure y

readNullOrUndefined :: forall a m. Index a => Monad m => a -> ParserT Foreign m (Maybe Foreign)
readNullOrUndefined k = hoist $ do
  x <- input
  y <- V.success (x ! k >>= F.readNullOrUndefined)
  pure y

readKeys :: forall m. Monad m => ParserT Foreign m (Array String)
readKeys = hoist $ do
  x <- input
  y <- V.success $ K.keys x
  pure y

string :: forall a b m. Index a => Monad m => a -> ParserT String m b -> ParserT Foreign m b
string k = V.output (readString k)

int :: forall a b m. Index a => Monad m => String -> ParserT Int m b -> ParserT Foreign m b
int k = V.output (readInt k)

boolean :: forall a b m. Index a => Monad m => String -> ParserT Boolean m b -> ParserT Foreign m b
boolean k = V.output (readBoolean k)

number :: forall a b m. Index a => Monad m => String -> ParserT Number m b -> ParserT Foreign m b
number k = V.output (readNumber k)

array :: forall a b m. Index a => Monad m => String -> ParserT (Array Foreign) m b -> ParserT Foreign m b
array k = V.output (readArray k)

null :: forall a b m. Index a => Monad m => String -> ParserT (Maybe Foreign) m b -> ParserT Foreign m b
null k = V.output (readNull k)

undefined :: forall a b m. Index a => Monad m => String -> ParserT (Maybe Foreign) m b -> ParserT Foreign m b
undefined k = V.output (readUndefined k)

nullOrUndefined :: forall a b m. Index a => Monad m => String -> ParserT (Maybe Foreign) m b -> ParserT Foreign m b
nullOrUndefined k = V.output (readNullOrUndefined k)

keys :: forall a m. Monad m => Traversable m => ParserT String m a -> ParserT Foreign m (Array a)
keys p = do
   u <- readKeys
   v <- pure (flip P.runParserT p <$> u)
   w <- pure $ T.sequence v
   x <- lift w
   y <- pure $ T.sequence x
   z <- V.right y
   pure z
