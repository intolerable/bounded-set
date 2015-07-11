module Data.Bounded.Set
  ( Set
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
import GHC.Show
import Prelude hiding (drop)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable

data Set a = Set !Int !(Seq a) !(Set.Set a)

instance Show a => Show (Set a) where
  showsPrec n (Set x _ t) =
    showParen (n > 10) $
      showString "fromList " . shows x . showString " " . shows (Set.toList t)

instance (Ord a, Binary a) => Binary (Set a) where
  get = fromList <$> get <*> get
  put (Set n q _) = put (n, q)

fromList :: Ord a => Int -> [a] -> Set a
fromList n xs = insertAll xs $ empty n

empty :: Int -> Set a
empty n = Set n Seq.empty Set.empty

member :: Ord a => a -> Set a -> Bool
member x (Set _ _ t) = Set.member x t

insert :: Ord a => a -> Set a -> Set a
insert a (Set maxLength q t) =
  if Set.member a t
    then Set maxLength q t
    else
      if Seq.length q < maxLength
        then Set maxLength (a <| q) (Set.insert a t)
        else replace a (Set maxLength q t)

insertAll :: Ord a => [a] -> Set a -> Set a
insertAll xs s = foldl (flip insert) s xs

replace :: Ord a => a -> Set a -> Set a
replace a s =
  let Set n q t = removeOldest s in
  Set n (a <| q) (Set.insert a t)

removeOldest :: Ord a => Set a -> Set a
removeOldest (Set n q t) =
  case viewr q of
    EmptyR -> Set n q t
    rest :> remove ->
      Set n rest (Set.delete remove t)

capacity :: Set a -> Int
capacity (Set x _ _) = x

size :: Set a -> Int
size (Set _ q _) = Seq.length q

resize :: Int -> Set a -> Set a
resize n (Set x q t) =
  if n > x
    then Set n q t
    else undefined
