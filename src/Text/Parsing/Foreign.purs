module Text.Parsing.Foreign
  ( keys
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
import Data.Either (Either(..))

import Data.Traversable (class Traversable)

import Foreign (Foreign)
import Foreign.Index (class Index, (!))

import Foreign as F
import Foreign.Keys as K

import Text.Parsing.Parser (ParserT, Parser)
import Text.Parsing.Parser as P

import Text.Parsing.Combinators.Validation as V

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

{-- | Applies a parser to each of the keys of the foreign parse input. --}
keys :: forall a m. Monad m => Traversable m => ParserT String m a -> ParserT Foreign m (Array a)
keys p = do
  x <- V.or (pure []) (V.apply p $ readKeys)
  case x of
    (Left y)  -> pure y
    (Right y) -> pure y

{-- | Applies a parser to a foreign value at a given index of the foreign parse input. --}
index :: forall a b m. Index a => Monad m => a -> ParserT Foreign m b -> ParserT Foreign m b
index k = V.output (readIndex k)

{-- | Applies a parser to a string at a given index of the foreign parse input. --}
string :: forall a b m. Index a => Monad m => a -> ParserT String m b -> ParserT Foreign m b
string k = V.output (readString k)

{-- | Applies a parser to a character at a given index of the foreign parse input. --}
char :: forall a b m. Index a => Monad m => a -> ParserT Char m b -> ParserT Foreign m b
char k = V.output (readChar k)

{-- | Applies a parser to an integer at a given index of the foreign parse input. --}
int :: forall a b m. Index a => Monad m => a -> ParserT Int m b -> ParserT Foreign m b
int k = V.output (readInt k)

{-- | Applies a parser to a boolean at a given index of the foreign parse input. --}
boolean :: forall a b m. Index a => Monad m => a -> ParserT Boolean m b -> ParserT Foreign m b
boolean k = V.output (readBoolean k)

{-- | Applies a parser to a number at a given index of the foreign parse input. --}
number :: forall a b m. Index a => Monad m => a -> ParserT Number m b -> ParserT Foreign m b
number k = V.output (readNumber k)

{-- | Applies a parser to an array of foreign values at a given index of the foreign parse input. --}
array :: forall a b m. Index a => Monad m => a -> ParserT (Array Foreign) m b -> ParserT Foreign m b
array k = V.output (readArray k)

{-- | Applies a parser to a nullable value at a given index of the foreign parse input. --}
null :: forall a b m. Index a => Monad m => a -> ParserT (Maybe Foreign) m b -> ParserT Foreign m b
null k = V.output (readNull k)

{-- | Applies a parser to an optionally undefined value at a given index of the foreign parse input. --}
undefined :: forall a b m. Index a => Monad m => a -> ParserT (Maybe Foreign) m b -> ParserT Foreign m b
undefined k = V.output (readUndefined k)

{-- | Applies a parser to an optionally null-or-undefined value at a given index of the foreign parse input. --}
nullOrUndefined :: forall a b m. Index a => Monad m => a -> ParserT (Maybe Foreign) m b -> ParserT Foreign m b
nullOrUndefined k = V.output (readNullOrUndefined k)
