{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- | A @Map@-like structure that contains up to one key-value pair
--
-- A 'Meep' is strict in the key.
--
-- @Meep k a@ is isomorphic to @Maybe (k, a)@ with 'maybeing' witnessing the isomorphism
module Data.Meep
#ifdef TEST
  ( Meep(..)
#else
  ( Meep
#endif
  , empty
  , singleton
  , size
  , null
  , fromMaybe
  , toMaybe
  , maybeing
  , intersection
  , intersectionWith
  , intersectionWithKey
  , keys
  , elems
  ) where

import Control.Applicative (pure, liftA2)
import Control.Lens
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor.Apply (Biapply(..))
import Data.Bitraversable (Bitraversable(..))
import Data.Monoid (mempty)
import Data.Data (Data, Typeable)
import Data.Foldable (Foldable)
import Data.Semigroup (Semigroup(..), Monoid(..))
import GHC.Generics (Generic)
import Prelude hiding (null, lookup)
#ifdef TEST
import Test.QuickCheck (Arbitrary(..))
#endif

{-# ANN module "HLint: ignore Use fromMaybe" #-}

-- | A Meep from key @k@ to value @a@
data Meep k a = Empty | Meep !k a
    deriving (Eq, Ord, Functor, Foldable, Traversable, Typeable, Data, Generic)

instance (Show k, Show a) => Show (Meep k a) where
  showsPrec p m = showParen (p > 10) (showString "fromMaybe " . shows (toMaybe m))

-- | 'Meep's intersection
instance (Eq k, Semigroup a) => Semigroup (Meep k a) where
  Empty    <> _          = Empty
  _        <> Empty      = Empty
  Meep k v <> Meep k' v' = bool Empty (Meep k (v <> v')) (k == k')

instance Bifunctor Meep where
  bimap _ _ Empty = Empty
  bimap f g (Meep k v) = Meep (f k) (g v)

instance Biapply Meep where
  Empty      <<.>> _        = Empty
  _          <<.>> Empty    = Empty
  Meep fk fv <<.>> Meep k v = Meep (fk k) (fv v)

instance Bifoldable Meep where
  bifoldMap _ _ Empty = mempty
  bifoldMap f g (Meep k v) = f k `mappend` g v

instance Bitraversable Meep where
  bitraverse _ _ Empty = pure Empty
  bitraverse f g (Meep k v) = liftA2 Meep (f k) (g v)

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

-- | /O(1)/. An empty 'Meep'
empty :: Meep k a
empty = Empty

-- | /O(1)/. A 'Meep' with a single key-value pair
singleton :: k -> a -> Meep k a
singleton = Meep

-- | /O(1)/. The size of the 'Meep'
--
-- >>> size empty
-- 0
--
-- >>> size (singleton 4 "foo")
-- 1
size :: Num b => Meep k a -> b
size = bool 1 0 . null

-- | /O(1)/. The emptiness check for the 'Meep'
--
-- >>> null empty
-- True
--
-- >>> null (singleton 4 "foo")
-- False
null :: Meep k a -> Bool
null Empty = True
null (Meep _ _) = False

-- | /O(1)/. Build the 'Meep'
--
-- @
-- fromMaybe ≡ view (from maybeing)
-- @
fromMaybe :: Maybe (k, a) -> Meep k a
fromMaybe = maybe Empty (uncurry Meep)

-- | /O(1)/. Destroy the 'Meep'
--
-- @
-- toMaybe ≡ view maybeing
-- @
toMaybe :: Meep k a -> Maybe (k, a)
toMaybe Empty      = Nothing
toMaybe (Meep k a) = Just (k, a)

-- | /O(1)/. A witness to
--
-- @
-- 'Meep' k v ≅ 'Maybe' (k, v)
-- @
--
-- >>> singleton 4 "foo" ^. maybeing
-- Just (4,"foo")
--
-- >>> Nothing ^. from maybeing
-- fromMaybe Nothing
maybeing :: Iso (Meep k v) (Meep k' v') (Maybe (k, v)) (Maybe (k', v'))
maybeing = iso toMaybe fromMaybe

-- | /O(1)/. Return all keys from the 'Meep'
--
-- >>> keys (singleton 4 "foo")
-- Just 4
--
-- >>> keys empty
-- Nothing
keys :: Meep k a -> Maybe k
keys Empty      = Nothing
keys (Meep k _) = Just k

-- | /O(1)/. Return all values from the 'Meep'
--
-- >>> elems (singleton 4 "foo")
-- Just "foo"
--
-- >>> elems empty
-- Nothing
elems :: Meep k a -> Maybe a
elems Empty      = Nothing
elems (Meep _ a) = Just a

-- | /O(1)/. Intersection of two 'Meep's
--
-- @
-- intersection ≡ 'intersectionWith' 'const'
-- @
intersection :: Eq k => Meep k a -> Meep k b -> Meep k a
intersection = intersectionWith const

-- | /O(1)/. Intersection of two 'Meep's with a combining function
--
-- >>> intersectionWith (+) (Meep "hello" 4) (Meep "hello" 7)
-- fromMaybe (Just ("hello",11))
--
-- >>> intersectionWith (+) (Meep "hello" 4) (Meep "bye" 7)
-- fromMaybe Nothing
--
-- >>> intersectionWith (+) Empty (Meep "hello" 7)
-- fromMaybe Nothing
--
-- @
-- intersectionWith f ≡ intersectionWithKey (const f)
-- @
intersectionWith :: Eq k => (a -> b -> c) -> Meep k a -> Meep k b -> Meep k c
intersectionWith f = intersectionWithKey (const f)

-- | /O(1)/. Intersection of two 'Meep's with a combining function
intersectionWithKey :: Eq k => (k -> a -> b -> c) -> Meep k a -> Meep k b -> Meep k c
intersectionWithKey f (Meep k a) (Meep k' b) | k == k' = Meep k (f k a b)
intersectionWithKey _ _          _                     = Empty

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
