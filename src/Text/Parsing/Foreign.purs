module Text.Parsing.Foreign
  ( keys
  , values
  , index
  , string
  , char
  , boolean
  , int
  , number
  , array
  , null
  , undefined
  , nullOrUndefined
  ) where

import Prelude

import Control.Monad.State.Class (gets)

import Data.Identity (Identity(..))
import Data.Maybe (Maybe)

import Data.Traversable (class Traversable)

import Foreign (Foreign)
import Foreign.Index (class Index, (!))

import Foreign as F
import Foreign.Keys as K

import Text.Parsing.Parser (ParserT, Parser)
import Text.Parsing.Parser as P

import Text.Parsing.Combinators.Validation as V

import FFI.Foreign.Object as O

readInput :: forall a m. Monad m => ParserT a m a
readInput = gets \(P.ParseState x _ _ ) -> x

hoistParser :: forall a m b. Monad m => Parser a b -> ParserT a m b
hoistParser =  P.hoistParserT (\(Identity x) -> pure x)

readIndex :: forall a m. Index a => Monad m => a -> ParserT Foreign m Foreign
readIndex k = hoistParser $ do
  x <- readInput
  y <- V.success (x ! k)
  pure y

readString :: forall a m. Index a => Monad m => a -> ParserT Foreign m String
readString k = hoistParser $ do
  x <- readInput
  y <- V.success (x ! k >>= F.readString)
  pure y

readChar :: forall a m. Index a => Monad m => a -> ParserT Foreign m Char
readChar k = hoistParser $ do
  x <- readInput
  y <- V.success (x ! k >>= F.readChar)
  pure y

readBoolean :: forall a m. Index a => Monad m => a -> ParserT Foreign m Boolean
readBoolean k = hoistParser $ do
  x <- readInput
  y <- V.success (x ! k >>= F.readBoolean)
  pure y

readInt :: forall a m. Index a => Monad m => a -> ParserT Foreign m Int
readInt k = hoistParser $ do
  x <- readInput
  y <- V.success (x ! k >>= F.readInt)
  pure y

readNumber :: forall a m. Index a => Monad m => a -> ParserT Foreign m Number
readNumber k = hoistParser $ do
  x <- readInput
  y <- V.success (x ! k >>= F.readNumber)
  pure y

readArray :: forall a m. Index a => Monad m => a -> ParserT Foreign m (Array Foreign)
readArray k = hoistParser $ do
  x <- readInput
  y <- V.success (x ! k >>= F.readArray)
  pure y

readNull :: forall a m. Index a => Monad m => a -> ParserT Foreign m (Maybe Foreign)
readNull k = hoistParser $ do
  x <- readInput
  y <- V.success (x ! k >>= F.readNull)
  pure y

readUndefined :: forall a m. Index a => Monad m => a -> ParserT Foreign m (Maybe Foreign)
readUndefined k = hoistParser $ do
  x <- readInput
  y <- V.success (x ! k >>= F.readUndefined)
  pure y

readNullOrUndefined :: forall a m. Index a => Monad m => a -> ParserT Foreign m (Maybe Foreign)
readNullOrUndefined k = hoistParser $ do
  x <- readInput
  y <- V.success (x ! k >>= F.readNullOrUndefined)
  pure y

readKeys :: forall m. Monad m => ParserT Foreign m (Array String)
readKeys = hoistParser $ do
  x <- readInput
  y <- V.success $ K.keys x
  pure y

keys :: forall a m. Monad m => Traversable m => ParserT String m a -> ParserT Foreign m (Array a)
keys = flip V.apply readKeys

values :: forall a m. Monad m => Traversable m => ParserT Foreign m a -> ParserT Foreign m (Array a)
values = flip V.apply (O.values <$> readInput)

index :: forall a b m. Index a => Monad m => a -> ParserT Foreign m b -> ParserT Foreign m b
index k = V.output (readIndex k)

string :: forall a b m. Index a => Monad m => a -> ParserT String m b -> ParserT Foreign m b
string k = V.output (readString k)

char :: forall a b m. Index a => Monad m => a -> ParserT Char m b -> ParserT Foreign m b
char k = V.output (readChar k)

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
