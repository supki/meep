{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Meep
#ifdef TEST
  ( Meep(..)
#else
  ( Meep
#endif
  , singleton
  , empty
  , size
  , null
  , fromMaybe
  , toMaybe
  , maybeing
  , keys
  , elems
  ) where

import Control.Applicative (pure)
import Control.Lens
import Data.Monoid (mempty)
import Data.Data (Data, Typeable)
import Data.Foldable (Foldable)
import GHC.Generics (Generic)
import Prelude hiding (null, lookup)
#ifdef TEST
import Test.QuickCheck (Arbitrary(..))
#endif

{-# ANN module "HLint: ignore Use fromMaybe" #-}

data Meep k a = Empty | Meep k a
    deriving (Eq, Ord, Functor, Foldable, Traversable, Typeable, Data, Generic)

instance (Show k, Show a) => Show (Meep k a) where
  showsPrec p m = showParen (p > 10) (showString "fromMaybe " . shows (toMaybe m))

instance Eq k => Ixed (Meep k a) where
  ix = ixAt

instance Eq k => At (Meep k a) where
  at k f m = indexed f k mv <&> \r -> case r of
    Nothing -> maybe m (const (delete k m)) mv
    Just v  -> insert k v m
   where
    mv = lookup k m

type instance Index (Meep k a) = k
type instance IxValue (Meep k a) = a

instance FunctorWithIndex k (Meep k) where
  imap _ Empty      = Empty
  imap f (Meep k a) = Meep k (f k a)

instance FoldableWithIndex k (Meep k) where
  ifoldMap _ Empty      = mempty
  ifoldMap f (Meep k a) = f k a

instance TraversableWithIndex k (Meep k) where
  itraverse _ Empty      = pure Empty
  itraverse f (Meep k a) = fmap (Meep k) (f k a)

instance AsEmpty (Meep k a) where
  _Empty = prism' (const Empty) (\x -> case x of Empty -> Just (); _ -> Nothing)

#ifdef TEST
instance (Arbitrary k, Arbitrary a) => Arbitrary (Meep k a) where
  arbitrary = fmap fromMaybe arbitrary
#endif

singleton :: k -> a -> Meep k a
singleton = Meep

empty :: Meep k a
empty = Empty

size :: Num b => Meep k a -> b
size = bool 1 0 . null

null :: Meep k a -> Bool
null Empty = True
null (Meep _ _) = False

fromMaybe :: Maybe (k, a) -> Meep k a
fromMaybe = maybe Empty (uncurry Meep)

toMaybe :: Meep k a -> Maybe (k, a)
toMaybe Empty      = Nothing
toMaybe (Meep k a) = Just (k, a)

maybeing :: Iso (Meep k v) (Meep k' v') (Maybe (k, v)) (Maybe (k', v'))
maybeing = iso toMaybe fromMaybe

keys :: Meep k a -> Maybe k
keys Empty      = Nothing
keys (Meep k _) = Just k

elems :: Meep k a -> Maybe a
elems Empty      = Nothing
elems (Meep _ a) = Just a

insert :: Eq k => k -> a -> Meep k a -> Meep k a
insert k a Empty         = Meep k a
insert k a x@(Meep k' _) = bool x (Meep k a) (k == k')

lookup :: Eq k => k -> Meep k a -> Maybe a
lookup _  Empty      = Nothing
lookup k' (Meep k a) = bool Nothing (Just a) (k == k')

delete :: Eq k => k -> Meep k a -> Meep k a
delete _  Empty        = Empty
delete k' x@(Meep k _) = bool x Empty (k == k')

bool :: a -> a -> Bool -> a
bool f t p = if p then t else f
