module Data.BoundedSet
  ( BoundedSet
  , empty
  , insert
  , insertAll
  , member
  , size
  , capacity
  , resize ) where

import Data.Binary
import Data.Monoid
import Data.Sequence hiding (drop, empty, fromList)
import Data.Set (Set)
import GHC.Show
import Prelude hiding (drop)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable

data BoundedSet a = BoundedSet !Int !(Seq a) !(Set a)

instance Show a => Show (BoundedSet a) where
  showsPrec n (BoundedSet x _ t) =
    showParen (n > 10) $
      showString "fromList " . shows x . showString " " . shows (Set.toList t)

instance (Ord a, Binary a) => Binary (BoundedSet a) where
  get = fromList <$> get <*> get
  put (BoundedSet n q _) = put (n, q)

fromList :: Ord a => Int -> [a] -> BoundedSet a
fromList n xs = insertAll xs $ empty n

empty :: Int -> BoundedSet a
empty n = BoundedSet n Seq.empty Set.empty

member :: Ord a => a -> BoundedSet a -> Bool
member x (BoundedSet _ _ t) = Set.member x t

insert :: Ord a => a -> BoundedSet a -> BoundedSet a
insert a (BoundedSet maxLength q t) =
  if Set.member a t
    then BoundedSet maxLength q t
    else
      if Seq.length q < maxLength
        then BoundedSet maxLength (a <| q) (Set.insert a t)
        else replace a (BoundedSet maxLength q t)

insertAll :: Ord a => [a] -> BoundedSet a -> BoundedSet a
insertAll xs s = foldl (flip insert) s xs

replace :: Ord a => a -> BoundedSet a -> BoundedSet a
replace a s =
  let BoundedSet n q t = removeOldest s in
  BoundedSet n (a <| q) (Set.insert a t)

removeOldest :: Ord a => BoundedSet a -> BoundedSet a
removeOldest (BoundedSet n q t) =
  case viewr q of
    EmptyR -> BoundedSet n q t
    rest :> remove ->
      BoundedSet n rest (Set.delete remove t)

capacity :: BoundedSet a -> Int
capacity (BoundedSet x _ _) = x

size :: BoundedSet a -> Int
size (BoundedSet _ q _) = Seq.length q

resize :: Int -> BoundedSet a -> BoundedSet a
resize n (BoundedSet x q t) =
  if n > x
    then BoundedSet n q t
    else undefined
