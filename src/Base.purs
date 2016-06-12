module Base
  ( module Prelude

  , module Data.Int
  , module Data.Maybe
  , module Data.Either
  , module Data.Tuple
  , module Data.Monoid
  , module Data.Unfoldable
  , module Data.Foldable
  , module Data.Traversable
  , module Data.List

  , module Control.Monad
  , module Control.MonadPlus
  , module Control.Alt
  , module Control.Plus

  , module Control.Monad.Eff
  , module Control.Monad.Eff.Console
  ) where

import Prelude

import Control.Alt (class Alt, class Functor, alt, map, void, ($>), (<#>), (<$), (<$>), (<|>))
import Control.Monad (class Applicative, class Apply, class Bind, class Functor, class Monad, ap, apply, bind, ifM, join, liftA1, liftM1, map, pure, unless, void, when, ($>), (*>), (<#>), (<$), (<$>), (<*), (<*>), (<=<), (=<<), (>=>), (>>=))
import Control.Monad.Eff (Eff, Pure, forE, foreachE, runPure, untilE, whileE)
import Control.Monad.Eff.Console (CONSOLE, error, errorShow, info, infoShow, log, logShow, warn, warnShow)
import Control.MonadPlus (class Alt, class Alternative, class Applicative, class Apply, class Bind, class Functor, class Monad, class MonadPlus, class MonadZero, class Plus, alt, ap, apply, bind, empty, guard, ifM, join, liftA1, liftM1, map, pure, unless, void, when, ($>), (*>), (<#>), (<$), (<$>), (<*), (<*>), (<=<), (<|>), (=<<), (>=>), (>>=))
import Control.Plus (class Alt, class Functor, class Plus, alt, empty, map, void, ($>), (<#>), (<$), (<$>), (<|>))
import Data.Either (Either(..), either, fromLeft, fromRight, isLeft, isRight)
import Data.Foldable (class Foldable, all, and, any, elem, find, fold, foldMap, foldMapDefaultL, foldMapDefaultR, foldl, foldlDefault, foldr, foldrDefault, for_, intercalate, maximum, maximumBy, minimum, minimumBy, notElem, oneOf, or, product, sequence_, sum, traverse_)
import Data.Int (Radix, binary, ceil, decimal, even, floor, fromNumber, fromString, fromStringAs, hexadecimal, octal, odd, radix, round, toNumber, toStringAs)
import Data.List (List(..), alterAt, catMaybes, concat, concatMap, delete, deleteAt, deleteBy, difference, drop, dropWhile, elemIndex, elemLastIndex, filter, filterM, findIndex, findLastIndex, foldM, fromFoldable, group, group', groupBy, head, index, init, insert, insertAt, insertBy, intersect, intersectBy, last, length, many, mapMaybe, mapWithIndex, modifyAt, nub, nubBy, null, range, reverse, slice, snoc, some, sort, sortBy, span, tail, take, takeWhile, toUnfoldable, transpose, uncons, union, unionBy, unzip, updateAt, zip, zipWith, zipWithA, (!!), (..), (:), (\\))
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe')
import Data.Monoid (class Monoid, class Semigroup, append, mempty, (<>))
import Data.Traversable (class Foldable, class Traversable, Accum, all, and, any, elem, find, fold, foldMap, foldMapDefaultL, foldMapDefaultR, foldl, foldlDefault, foldr, foldrDefault, for, for_, intercalate, mapAccumL, mapAccumR, maximum, maximumBy, minimum, minimumBy, notElem, oneOf, or, product, scanl, scanr, sequence, sequenceDefault, sequence_, sum, traverse, traverseDefault, traverse_)
import Data.Tuple (Tuple(..), curry, fst, lookup, snd, swap, uncurry)
import Data.Unfoldable (class Unfoldable, none, replicateA, singleton, unfoldr)
