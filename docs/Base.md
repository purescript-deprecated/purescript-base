## Module Base


### Re-exported from Control.Monad.Eff:

#### `Pure`

``` purescript
type Pure a = Eff () a
```

The `Pure` type synonym represents _pure_ computations, i.e. ones in which
all effects have been handled.

The `runPure` function can be used to run pure computations and obtain
their result.

#### `Eff`

``` purescript
data Eff :: # ! -> * -> *
```

The `Eff` type constructor is used to represent _native_ effects.

See [Handling Native Effects with the Eff Monad](http://www.purescript.org/learn/eff/)
for more details.

The first type parameter is a row of effects which represents the contexts
in which a computation can be run, and the second type parameter is the
return type.

##### Instances
``` purescript
Functor (Eff e)
Apply (Eff e)
Applicative (Eff e)
Bind (Eff e)
Monad (Eff e)
```

#### `whileE`

``` purescript
whileE :: forall e a. Eff e Boolean -> Eff e a -> Eff e Unit
```

Loop while a condition is `true`.

`whileE b m` is effectful computation which runs the effectful computation
`b`. If its result is `true`, it runs the effectful computation `m` and
loops. If not, the computation ends.

#### `untilE`

``` purescript
untilE :: forall e. Eff e Boolean -> Eff e Unit
```

Loop until a condition becomes `true`.

`untilE b` is an effectful computation which repeatedly runs the effectful
computation `b`, until its return value is `true`.

#### `runPure`

``` purescript
runPure :: forall a. Pure a -> a
```

Run a pure computation and return its result.

#### `foreachE`

``` purescript
foreachE :: forall e a. Array a -> (a -> Eff e Unit) -> Eff e Unit
```

Loop over an array of values.

`foreach xs f` runs the computation returned by the function `f` for each
of the inputs `xs`.

#### `forE`

``` purescript
forE :: forall e. Int -> Int -> (Int -> Eff e Unit) -> Eff e Unit
```

Loop over a consecutive collection of numbers.

`forE lo hi f` runs the computation returned by the function `f` for each
of the inputs between `lo` (inclusive) and `hi` (exclusive).

### Re-exported from Control.Monad.Eff.Console:

#### `CONSOLE`

``` purescript
data CONSOLE :: !
```

The `CONSOLE` effect represents those computations which write to the
console.

#### `warnShow`

``` purescript
warnShow :: forall a eff. Show a => a -> Eff (console :: CONSOLE | eff) Unit
```

Write an warning value to the console, using its `Show` instance to produce
a `String`.

#### `warn`

``` purescript
warn :: forall eff. String -> Eff (console :: CONSOLE | eff) Unit
```

Write an warning to the console.

#### `logShow`

``` purescript
logShow :: forall a eff. Show a => a -> Eff (console :: CONSOLE | eff) Unit
```

Write a value to the console, using its `Show` instance to produce a
`String`.

#### `log`

``` purescript
log :: forall eff. String -> Eff (console :: CONSOLE | eff) Unit
```

Write a message to the console.

#### `infoShow`

``` purescript
infoShow :: forall a eff. Show a => a -> Eff (console :: CONSOLE | eff) Unit
```

Write an info value to the console, using its `Show` instance to produce a
`String`.

#### `info`

``` purescript
info :: forall eff. String -> Eff (console :: CONSOLE | eff) Unit
```

Write an info message to the console.

#### `errorShow`

``` purescript
errorShow :: forall a eff. Show a => a -> Eff (console :: CONSOLE | eff) Unit
```

Write an error value to the console, using its `Show` instance to produce a
`String`.

#### `error`

``` purescript
error :: forall eff. String -> Eff (console :: CONSOLE | eff) Unit
```

Write an error to the console.

### Re-exported from Control.MonadPlus:

#### `Alternative`

``` purescript
class (Applicative f, Plus f) <= Alternative f
```

The `Alternative` type class has no members of its own; it just specifies
that the type constructor has both `Applicative` and `Plus` instances.

Types which have `Alternative` instances should also satisfy the following
laws:

- Distributivity: `(f <|> g) <*> x == (f <*> x) <|> (g <*> x)`
- Annihilation: `empty <*> f = empty`

##### Instances
``` purescript
Alternative Array
```

#### `MonadPlus`

``` purescript
class (MonadZero m) <= MonadPlus m
```

The `MonadPlus` type class has no members of its own but extends
`MonadZero` with an additional law:

- Distributivity: `(x <|> y) >>= f == (x >>= f) <|> (y >>= f)`

##### Instances
``` purescript
MonadPlus Array
```

#### `MonadZero`

``` purescript
class (Monad m, Alternative m) <= MonadZero m
```

The `MonadZero` type class has no members of its own; it just specifies
that the type has both `Monad` and `Alternative` instances.

Types which have `MonadZero` instances should also satisfy the following
laws:

- Annihilation: `empty >>= f = empty`

##### Instances
``` purescript
MonadZero Array
```

#### `guard`

``` purescript
guard :: forall m. MonadZero m => Boolean -> m Unit
```

Fail using `Plus` if a condition does not hold, or
succeed using `Monad` if it does.

For example:

```purescript
import Data.Array

factors :: Number -> Array Number
factors n = do
  a <- 1 .. n
  b <- 1 .. a
  guard $ a * b == n
  pure a
```

### Re-exported from Control.Plus:

#### `Alt`

``` purescript
class (Functor f) <= Alt f where
  alt :: forall a. f a -> f a -> f a
```

The `Alt` type class identifies an associative operation on a type
constructor.  It is similar to `Semigroup`, except that it applies to
types of kind `* -> *`, like `Array` or `List`, rather than concrete types
`String` or `Number`.

`Alt` instances are required to satisfy the following laws:

- Associativity: `(x <|> y) <|> z == x <|> (y <|> z)`
- Distributivity: `f <$> (x <|> y) == (f <$> x) <|> (f <$> y)`

For example, the `Array` (`[]`) type is an instance of `Alt`, where
`(<|>)` is defined to be concatenation.

##### Instances
``` purescript
Alt Array
```

#### `Plus`

``` purescript
class (Alt f) <= Plus f where
  empty :: forall a. f a
```

The `Plus` type class extends the `Alt` type class with a value that
should be the left and right identity for `(<|>)`.

It is similar to `Monoid`, except that it applies to types of
kind `* -> *`, like `Array` or `List`, rather than concrete types like
`String` or `Number`.

`Plus` instances should satisfy the following laws:

- Left identity: `empty <|> x == x`
- Right identity: `x <|> empty == x`
- Annihilation: `f <$> empty == empty`

##### Instances
``` purescript
Plus Array
```

#### `(<|>)`

``` purescript
infixl 3 Control.Alt.alt as <|>
```

### Re-exported from Data.Either:

#### `Either`

``` purescript
data Either a b
  = Left a
  | Right b
```

The `Either` type is used to represent a choice between two types of value.

A common use case for `Either` is error handling, where `Left` is used to
carry an error value and `Right` is used to carry a success value.

##### Instances
``` purescript
Functor (Either a)
Invariant (Either a)
Bifunctor Either
Apply (Either e)
Applicative (Either e)
Alt (Either e)
Bind (Either e)
Monad (Either e)
Extend (Either e)
(Show a, Show b) => Show (Either a b)
(Eq a, Eq b) => Eq (Either a b)
(Ord a, Ord b) => Ord (Either a b)
(Bounded a, Bounded b) => Bounded (Either a b)
Foldable (Either a)
Bifoldable Either
Traversable (Either a)
Bitraversable Either
(Semiring b) => Semiring (Either a b)
(Semigroup b) => Semigroup (Either a b)
```

#### `isRight`

``` purescript
isRight :: forall a b. Either a b -> Boolean
```

Returns `true` when the `Either` value was constructed with `Right`.

#### `isLeft`

``` purescript
isLeft :: forall a b. Either a b -> Boolean
```

Returns `true` when the `Either` value was constructed with `Left`.

#### `fromRight`

``` purescript
fromRight :: forall a b. Partial => Either a b -> b
```

A partial function that extracts the value from the `Right` data constructor.
Passing a `Left` to `fromRight` will throw an error at runtime.

#### `fromLeft`

``` purescript
fromLeft :: forall a b. Partial => Either a b -> a
```

A partial function that extracts the value from the `Left` data constructor.
Passing a `Right` to `fromLeft` will throw an error at runtime.

#### `either`

``` purescript
either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c
```

Takes two functions and an `Either` value, if the value is a `Left` the
inner value is applied to the first function, if the value is a `Right`
the inner value is applied to the second function.

``` purescript
either f g (Left x) == f x
either f g (Right y) == g y
```

### Re-exported from Data.Int:

#### `Radix`

``` purescript
newtype Radix
```

The number of unique digits (including zero) used to represent integers in
a specific base.

#### `toStringAs`

``` purescript
toStringAs :: Radix -> Int -> String
```

#### `toNumber`

``` purescript
toNumber :: Int -> Number
```

Converts an `Int` value back into a `Number`. Any `Int` is a valid `Number`
so there is no loss of precision with this function.

#### `round`

``` purescript
round :: Number -> Int
```

Convert a `Number` to an `Int`, by taking the nearest integer to the
argument. Values outside the `Int` range are clamped.

#### `radix`

``` purescript
radix :: Int -> Maybe Radix
```

Create a `Radix` from a number between 2 and 36.

#### `odd`

``` purescript
odd :: Int -> Boolean
```

The negation of `even`.

``` purescript
odd 0 == false
odd 1 == false
```

#### `octal`

``` purescript
octal :: Radix
```

The base-8 system.

#### `hexadecimal`

``` purescript
hexadecimal :: Radix
```

The base-16 system.

#### `fromStringAs`

``` purescript
fromStringAs :: Radix -> String -> Maybe Int
```

Like `fromString`, but the integer can be specified in a different base.

Example:
``` purs
fromStringAs binary      "100" == Just 4
fromStringAs hexadecimal "ff"  == Just 255
```

#### `fromString`

``` purescript
fromString :: String -> Maybe Int
```

Reads an `Int` from a `String` value. The number must parse as an integer
and fall within the valid range of values for the `Int` type, otherwise
`Nothing` is returned.

#### `fromNumber`

``` purescript
fromNumber :: Number -> Maybe Int
```

Creates an `Int` from a `Number` value. The number must already be an
integer and fall within the valid range of values for the `Int` type
otherwise `Nothing` is returned.

#### `floor`

``` purescript
floor :: Number -> Int
```

Convert a `Number` to an `Int`, by taking the closest integer equal to or
less than the argument. Values outside the `Int` range are clamped.

#### `even`

``` purescript
even :: Int -> Boolean
```

Returns whether an `Int` is an even number.

``` purescript
even 0 == true
even 1 == false
```

#### `decimal`

``` purescript
decimal :: Radix
```

The base-10 system.

#### `ceil`

``` purescript
ceil :: Number -> Int
```

Convert a `Number` to an `Int`, by taking the closest integer equal to or
greater than the argument. Values outside the `Int` range are clamped.

#### `binary`

``` purescript
binary :: Radix
```

The base-2 system.

### Re-exported from Data.List:

#### `List`

``` purescript
data List a
  = Nil
  | Cons a (List a)
```

A strict linked list.

A list is either empty (represented by the `Nil` constructor) or non-empty, in
which case it consists of a head element, and another list (represented by the
`Cons` constructor).

##### Instances
``` purescript
(Generic a) => Generic (List a)
(Show a) => Show (List a)
(Eq a) => Eq (List a)
(Ord a) => Ord (List a)
Semigroup (List a)
Monoid (List a)
Functor List
Foldable List
Unfoldable List
Traversable List
Apply List
Applicative List
Bind List
Monad List
Alt List
Plus List
Alternative List
MonadZero List
MonadPlus List
```

#### `zipWithA`

``` purescript
zipWithA :: forall m a b c. Applicative m => (a -> b -> m c) -> List a -> List b -> m (List c)
```

A generalization of `zipWith` which accumulates results in some `Applicative`
functor.

#### `zipWith`

``` purescript
zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c
```

Apply a function to pairs of elements at the same positions in two lists,
collecting the results in a new list.

If one list is longer, elements will be discarded from the longer list.

For example

```purescript
zipWith (*) (1 : 2 : 3 : Nil) (4 : 5 : 6 : 7 Nil) == 4 : 10 : 18 : Nil
```

Running time: `O(min(m, n))`

#### `zip`

``` purescript
zip :: forall a b. List a -> List b -> List (Tuple a b)
```

Collect pairs of elements at the same positions in two lists.

Running time: `O(min(m, n))`

#### `updateAt`

``` purescript
updateAt :: forall a. Int -> a -> List a -> Maybe (List a)
```

Update the element at the specified index, returning a new
list or `Nothing` if the index is out-of-bounds.

Running time: `O(n)`

#### `unzip`

``` purescript
unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
```

Transforms a list of pairs into a list of first components and a list of
second components.

#### `unionBy`

``` purescript
unionBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
```

Calculate the union of two lists, using the specified
function to determine equality of elements.

Running time: `O(n^2)`

#### `union`

``` purescript
union :: forall a. Eq a => List a -> List a -> List a
```

Calculate the union of two lists.

Running time: `O(n^2)`

#### `uncons`

``` purescript
uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
```

Break a list into its first element, and the remaining elements,
or `Nothing` if the list is empty.

Running time: `O(1)`

#### `transpose`

``` purescript
transpose :: forall a. List (List a) -> List (List a)
```

The 'transpose' function transposes the rows and columns of its argument.
For example,

    transpose ((1:2:3:Nil) : (4:5:6:Nil) : Nil) ==
      ((1:4:Nil) : (2:5:Nil) : (3:6:Nil) : Nil)

If some of the rows are shorter than the following rows, their elements are skipped:

    transpose ((10:11:Nil) : (20:Nil) : Nil : (30:31:32:Nil) : Nil) ==
      ((10:20:30:Nil) : (11:31:Nil) : (32:Nil) : Nil)

#### `toUnfoldable`

``` purescript
toUnfoldable :: forall f a. Unfoldable f => List a -> f a
```

Convert a list into any unfoldable structure.

Running time: `O(n)`

#### `takeWhile`

``` purescript
takeWhile :: forall a. (a -> Boolean) -> List a -> List a
```

Take those elements from the front of a list which match a predicate.

Running time (worst case): `O(n)`

#### `take`

``` purescript
take :: forall a. Int -> List a -> List a
```

Take the specified number of elements from the front of a list.

Running time: `O(n)` where `n` is the number of elements to take.

#### `tail`

``` purescript
tail :: forall a. List a -> Maybe (List a)
```

Get all but the first element of a list, or `Nothing` if the list is empty.

Running time: `O(1)`

#### `span`

``` purescript
span :: forall a. (a -> Boolean) -> List a -> { init :: List a, rest :: List a }
```

Split a list into two parts:

1. the longest initial segment for which all elements satisfy the specified predicate
2. the remaining elements

For example,

```purescript
span (\n -> n % 2 == 1) (1 : 3 : 2 : 4 : 5 : Nil) == { init: (1 : 3 : Nil), rest: (2 : 4 : 5 : Nil) }
```

Running time: `O(n)`

#### `sortBy`

``` purescript
sortBy :: forall a. (a -> a -> Ordering) -> List a -> List a
```

Sort the elements of a list in increasing order, where elements are
compared using the specified ordering.

#### `sort`

``` purescript
sort :: forall a. Ord a => List a -> List a
```

Sort the elements of an list in increasing order.

#### `some`

``` purescript
some :: forall f a. (Alternative f, Lazy (f (List a))) => f a -> f (List a)
```

Attempt a computation multiple times, requiring at least one success.

The `Lazy` constraint is used to generate the result lazily, to ensure
termination.

#### `snoc`

``` purescript
snoc :: forall a. List a -> a -> List a
```

Append an element to the end of a list, creating a new list.

Running time: `O(2n)`

#### `slice`

``` purescript
slice :: forall a. Int -> Int -> List a -> List a
```

Extract a sublist by a start and end index.

#### `reverse`

``` purescript
reverse :: forall a. List a -> List a
```

Reverse a list.

Running time: `O(n)`

#### `range`

``` purescript
range :: Int -> Int -> List Int
```

Create a list containing a range of integers, including both endpoints.

#### `null`

``` purescript
null :: forall a. List a -> Boolean
```

Test whether a list is empty.

Running time: `O(1)`

#### `nubBy`

``` purescript
nubBy :: forall a. (a -> a -> Boolean) -> List a -> List a
```

Remove duplicate elements from a list, using the specified
function to determine equality of elements.

Running time: `O(n^2)`

#### `nub`

``` purescript
nub :: forall a. Eq a => List a -> List a
```

Remove duplicate elements from a list.

Running time: `O(n^2)`

#### `modifyAt`

``` purescript
modifyAt :: forall a. Int -> (a -> a) -> List a -> Maybe (List a)
```

Update the element at the specified index by applying a function to
the current value, returning a new list or `Nothing` if the index is
out-of-bounds.

Running time: `O(n)`

#### `mapWithIndex`

``` purescript
mapWithIndex :: forall a b. (a -> Int -> b) -> List a -> List b
```

Apply a function to each element and its index in a list starting at 0.

#### `mapMaybe`

``` purescript
mapMaybe :: forall a b. (a -> Maybe b) -> List a -> List b
```

Apply a function to each element in a list, keeping only the results which
contain a value.

Running time: `O(n)`

#### `many`

``` purescript
many :: forall f a. (Alternative f, Lazy (f (List a))) => f a -> f (List a)
```

Attempt a computation multiple times, returning as many successful results
as possible (possibly zero).

The `Lazy` constraint is used to generate the result lazily, to ensure
termination.

#### `length`

``` purescript
length :: forall a. List a -> Int
```

Get the length of a list

Running time: `O(n)`

#### `last`

``` purescript
last :: forall a. List a -> Maybe a
```

Get the last element in a list, or `Nothing` if the list is empty.

Running time: `O(n)`.

#### `intersectBy`

``` purescript
intersectBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a
```

Calculate the intersection of two lists, using the specified
function to determine equality of elements.

Running time: `O(n^2)`

#### `intersect`

``` purescript
intersect :: forall a. Eq a => List a -> List a -> List a
```

Calculate the intersection of two lists.

Running time: `O(n^2)`

#### `insertBy`

``` purescript
insertBy :: forall a. (a -> a -> Ordering) -> a -> List a -> List a
```

Insert an element into a sorted list, using the specified function to
determine the ordering of elements.

Running time: `O(n)`

#### `insertAt`

``` purescript
insertAt :: forall a. Int -> a -> List a -> Maybe (List a)
```

Insert an element into a list at the specified index, returning a new
list or `Nothing` if the index is out-of-bounds.

Running time: `O(n)`

#### `insert`

``` purescript
insert :: forall a. Ord a => a -> List a -> List a
```

Insert an element into a sorted list.

Running time: `O(n)`

#### `init`

``` purescript
init :: forall a. List a -> Maybe (List a)
```

Get all but the last element of a list, or `Nothing` if the list is empty.

Running time: `O(n)`

#### `index`

``` purescript
index :: forall a. List a -> Int -> Maybe a
```

Get the element at the specified index, or `Nothing` if the index is out-of-bounds.

Running time: `O(n)` where `n` is the required index.

#### `head`

``` purescript
head :: forall a. List a -> Maybe a
```

Get the first element in a list, or `Nothing` if the list is empty.

Running time: `O(1)`.

#### `groupBy`

``` purescript
groupBy :: forall a. (a -> a -> Boolean) -> List a -> List (List a)
```

Group equal, consecutive elements of a list into lists, using the specified
equivalence relation to determine equality.

Running time: `O(n)`

#### `group'`

``` purescript
group' :: forall a. Ord a => List a -> List (List a)
```

Sort and then group the elements of a list into lists.

```purescript
group' [1,1,2,2,1] == [[1,1,1],[2,2]]
```

#### `group`

``` purescript
group :: forall a. Eq a => List a -> List (List a)
```

Group equal, consecutive elements of a list into lists.

For example,

```purescript
group (1 : 1 : 2 : 2 : 1 : Nil) == (1 : 1 : Nil) : (2 : 2 : Nil) : (1 : Nil) : Nil
```

Running time: `O(n)`

#### `fromFoldable`

``` purescript
fromFoldable :: forall f a. Foldable f => f a -> List a
```

Construct a list from a foldable structure.

Running time: `O(n)`

#### `foldM`

``` purescript
foldM :: forall m a b. Monad m => (a -> b -> m a) -> a -> List b -> m a
```

Perform a fold using a monadic step function.

#### `findLastIndex`

``` purescript
findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
```

Find the last index for which a predicate holds.

#### `findIndex`

``` purescript
findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
```

Find the first index for which a predicate holds.

#### `filterM`

``` purescript
filterM :: forall a m. Monad m => (a -> m Boolean) -> List a -> m (List a)
```

Filter where the predicate returns a monadic `Boolean`.

For example:

```purescript
powerSet :: forall a. [a] -> [[a]]
powerSet = filterM (const [true, false])
```

#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> List a -> List a
```

Filter a list, keeping the elements which satisfy a predicate function.

Running time: `O(n)`

#### `elemLastIndex`

``` purescript
elemLastIndex :: forall a. Eq a => a -> List a -> Maybe Int
```

Find the index of the last element equal to the specified element.

#### `elemIndex`

``` purescript
elemIndex :: forall a. Eq a => a -> List a -> Maybe Int
```

Find the index of the first element equal to the specified element.

#### `dropWhile`

``` purescript
dropWhile :: forall a. (a -> Boolean) -> List a -> List a
```

Drop those elements from the front of a list which match a predicate.

Running time (worst case): `O(n)`

#### `drop`

``` purescript
drop :: forall a. Int -> List a -> List a
```

Drop the specified number of elements from the front of a list.

Running time: `O(n)` where `n` is the number of elements to drop.

#### `difference`

``` purescript
difference :: forall a. Eq a => List a -> List a -> List a
```

Delete the first occurrence of each element in the second list from the first list.

Running time: `O(n^2)`

#### `deleteBy`

``` purescript
deleteBy :: forall a. (a -> a -> Boolean) -> a -> List a -> List a
```

Delete the first occurrence of an element from a list, using the specified
function to determine equality of elements.

Running time: `O(n)`

#### `deleteAt`

``` purescript
deleteAt :: forall a. Int -> List a -> Maybe (List a)
```

Delete an element from a list at the specified index, returning a new
list or `Nothing` if the index is out-of-bounds.

Running time: `O(n)`

#### `delete`

``` purescript
delete :: forall a. Eq a => a -> List a -> List a
```

Delete the first occurrence of an element from a list.

Running time: `O(n)`

#### `concatMap`

``` purescript
concatMap :: forall a b. (a -> List b) -> List a -> List b
```

Apply a function to each element in a list, and flatten the results
into a single, new list.

Running time: `O(n)`, where `n` is the total number of elements.

#### `concat`

``` purescript
concat :: forall a. List (List a) -> List a
```

Flatten a list of lists.

Running time: `O(n)`, where `n` is the total number of elements.

#### `catMaybes`

``` purescript
catMaybes :: forall a. List (Maybe a) -> List a
```

Filter a list of optional values, keeping only the elements which contain
a value.

#### `alterAt`

``` purescript
alterAt :: forall a. Int -> (a -> Maybe a) -> List a -> Maybe (List a)
```

Update or delete the element at the specified index by applying a
function to the current value, returning a new list or `Nothing` if the
index is out-of-bounds.

Running time: `O(n)`

#### `(\\)`

``` purescript
infix 5 difference as \\
```

#### `(:)`

``` purescript
infixr 6 Cons as :
```

An infix alias for `Cons`; attaches an element to the front of
a list.

Running time: `O(1)`

#### `(..)`

``` purescript
infix 8 range as ..
```

An infix synonym for `range`.

#### `(!!)`

``` purescript
infixl 8 index as !!
```

An infix synonym for `index`.

### Re-exported from Data.Maybe:

#### `Maybe`

``` purescript
data Maybe a
  = Just a
  | Nothing
```

The `Maybe` type is used to represent optional values and can be seen as
something like a type-safe `null`, where `Nothing` is `null` and `Just x`
is the non-null value `x`.

##### Instances
``` purescript
Functor Maybe
Apply Maybe
Applicative Maybe
Alt Maybe
Plus Maybe
Alternative Maybe
Bind Maybe
Monad Maybe
MonadZero Maybe
Extend Maybe
Invariant Maybe
(Semigroup a) => Semigroup (Maybe a)
(Semigroup a) => Monoid (Maybe a)
(Eq a) => Eq (Maybe a)
(Ord a) => Ord (Maybe a)
(Bounded a) => Bounded (Maybe a)
(Show a) => Show (Maybe a)
```

#### `maybe'`

``` purescript
maybe' :: forall a b. (Unit -> b) -> (a -> b) -> Maybe a -> b
```

Similar to `maybe` but for use in cases where the default value may be
expensive to compute. As PureScript is not lazy, the standard `maybe` has
to evaluate the default value before returning the result, whereas here
the value is only computed when the `Maybe` is known to be `Nothing`.

``` purescript
maybe' (\_ -> x) f Nothing == x
maybe' (\_ -> x) f (Just y) == f y
```

#### `maybe`

``` purescript
maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
```

Takes a default value, a function, and a `Maybe` value. If the `Maybe`
value is `Nothing` the default value is returned, otherwise the function
is applied to the value inside the `Just` and the result is returned.

``` purescript
maybe x f Nothing == x
maybe x f (Just y) == f y
```

#### `isNothing`

``` purescript
isNothing :: forall a. Maybe a -> Boolean
```

Returns `true` when the `Maybe` value is `Nothing`.

#### `isJust`

``` purescript
isJust :: forall a. Maybe a -> Boolean
```

Returns `true` when the `Maybe` value was constructed with `Just`.

#### `fromMaybe'`

``` purescript
fromMaybe' :: forall a. (Unit -> a) -> Maybe a -> a
```

Similar to `fromMaybe` but for use in cases where the default value may be
expensive to compute. As PureScript is not lazy, the standard `fromMaybe`
has to evaluate the default value before returning the result, whereas here
the value is only computed when the `Maybe` is known to be `Nothing`.

``` purescript
fromMaybe' (\_ -> x) Nothing == x
fromMaybe' (\_ -> x) (Just y) == y
```

#### `fromMaybe`

``` purescript
fromMaybe :: forall a. a -> Maybe a -> a
```

Takes a default value, and a `Maybe` value. If the `Maybe` value is
`Nothing` the default value is returned, otherwise the value inside the
`Just` is returned.

``` purescript
fromMaybe x Nothing == x
fromMaybe x (Just y) == y
```

#### `fromJust`

``` purescript
fromJust :: forall a. Partial => Maybe a -> a
```

A partial function that extracts the value from the `Just` data
constructor. Passing `Nothing` to `fromJust` will throw an error at
runtime.

### Re-exported from Data.Monoid:

#### `Monoid`

``` purescript
class (Semigroup m) <= Monoid m where
  mempty :: m
```

A `Monoid` is a `Semigroup` with a value `mempty`, which is both a
left and right unit for the associative operation `<>`:

```text
forall x. mempty <> x = x <> mempty = x
```

`Monoid`s are commonly used as the result of fold operations, where
`<>` is used to combine individual results, and `mempty` gives the result
of folding an empty collection of elements.

##### Instances
``` purescript
Monoid Unit
(Monoid b) => Monoid (a -> b)
Monoid String
Monoid (Array a)
```

### Re-exported from Data.Traversable:

#### `Accum`

``` purescript
type Accum s a = { accum :: s, value :: a }
```

#### `Foldable`

``` purescript
class Foldable f where
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
```

`Foldable` represents data structures which can be _folded_.

- `foldr` folds a structure from the right
- `foldl` folds a structure from the left
- `foldMap` folds a structure by accumulating values in a `Monoid`

Default implementations are provided by the following functions:

- `foldrDefault`
- `foldlDefault`
- `foldMapDefaultR`
- `foldMapDefaultL`

Note: some combinations of the default implementations are unsafe to
use together - causing a non-terminating mutually recursive cycle.
These combinations are documented per function.

##### Instances
``` purescript
Foldable Array
Foldable Maybe
Foldable First
Foldable Last
Foldable Additive
Foldable Dual
Foldable Disj
Foldable Conj
Foldable Multiplicative
```

#### `Traversable`

``` purescript
class (Functor t, Foldable t) <= Traversable t where
  traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
  sequence :: forall a m. Applicative m => t (m a) -> m (t a)
```

`Traversable` represents data structures which can be _traversed_,
accumulating results and effects in some `Applicative` functor.

- `traverse` runs an action for every element in a data structure,
  and accumulates the results.
- `sequence` runs the actions _contained_ in a data structure,
  and accumulates the results.

The `traverse` and `sequence` functions should be compatible in the
following sense:

- `traverse f xs = sequence (f <$> xs)`
- `sequence = traverse id`

`Traversable` instances should also be compatible with the corresponding
`Foldable` instances, in the following sense:

- `foldMap f = runConst <<< traverse (Const <<< f)`

Default implementations are provided by the following functions:

- `traverseDefault`
- `sequenceDefault`

##### Instances
``` purescript
Traversable Array
Traversable Maybe
Traversable First
Traversable Last
Traversable Additive
Traversable Dual
Traversable Conj
Traversable Disj
Traversable Multiplicative
```

#### `traverse_`

``` purescript
traverse_ :: forall a b f m. (Applicative m, Foldable f) => (a -> m b) -> f a -> m Unit
```

Traverse a data structure, performing some effects encoded by an
`Applicative` functor at each value, ignoring the final result.

For example:

```purescript
traverse_ print [1, 2, 3]
```

#### `traverseDefault`

``` purescript
traverseDefault :: forall t a b m. (Traversable t, Applicative m) => (a -> m b) -> t a -> m (t b)
```

A default implementation of `traverse` using `sequence` and `map`.

#### `sum`

``` purescript
sum :: forall a f. (Foldable f, Semiring a) => f a -> a
```

Find the sum of the numeric values in a data structure.

#### `sequence_`

``` purescript
sequence_ :: forall a f m. (Applicative m, Foldable f) => f (m a) -> m Unit
```

Perform all of the effects in some data structure in the order
given by the `Foldable` instance, ignoring the final result.

For example:

```purescript
sequence_ [ trace "Hello, ", trace " world!" ]
```

#### `sequenceDefault`

``` purescript
sequenceDefault :: forall t a m. (Traversable t, Applicative m) => t (m a) -> m (t a)
```

A default implementation of `sequence` using `traverse`.

#### `scanr`

``` purescript
scanr :: forall a b f. Traversable f => (a -> b -> b) -> b -> f a -> f b
```

Fold a data structure from the right, keeping all intermediate results
instead of only the final result. Note that the initial value does not
appear in the result (unlike Haskell's `Prelude.scanr`).

```purescript
scanr (+) 0  [1,2,3] = [1,3,6]
scanr (flip (-)) 10 [1,2,3] = [4,5,7]
```

#### `scanl`

``` purescript
scanl :: forall a b f. Traversable f => (b -> a -> b) -> b -> f a -> f b
```

Fold a data structure from the left, keeping all intermediate results
instead of only the final result. Note that the initial value does not
appear in the result (unlike Haskell's `Prelude.scanl`).

```purescript
scanl (+) 0  [1,2,3] = [1,3,6]
scanl (-) 10 [1,2,3] = [9,7,4]
```

#### `product`

``` purescript
product :: forall a f. (Foldable f, Semiring a) => f a -> a
```

Find the product of the numeric values in a data structure.

#### `or`

``` purescript
or :: forall a f. (Foldable f, BooleanAlgebra a) => f a -> a
```

The disjunction of all the values in a data structure. When specialized
to `Boolean`, this function will test whether any of the values in a data
structure is `true`.

#### `oneOf`

``` purescript
oneOf :: forall f g a. (Foldable f, Plus g) => f (g a) -> g a
```

Combines a collection of elements using the `Alt` operation.

#### `notElem`

``` purescript
notElem :: forall a f. (Foldable f, Eq a) => a -> f a -> Boolean
```

Test whether a value is not an element of a data structure.

#### `minimumBy`

``` purescript
minimumBy :: forall a f. Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
```

Find the smallest element of a structure, according to a given comparison
function. The comparison function should represent a total ordering (see
the `Ord` type class laws); if it does not, the behaviour is undefined.

#### `minimum`

``` purescript
minimum :: forall a f. (Ord a, Foldable f) => f a -> Maybe a
```

Find the smallest element of a structure, according to its `Ord` instance.

#### `maximumBy`

``` purescript
maximumBy :: forall a f. Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
```

Find the largest element of a structure, according to a given comparison
function. The comparison function should represent a total ordering (see
the `Ord` type class laws); if it does not, the behaviour is undefined.

#### `maximum`

``` purescript
maximum :: forall a f. (Ord a, Foldable f) => f a -> Maybe a
```

Find the largest element of a structure, according to its `Ord` instance.

#### `mapAccumR`

``` purescript
mapAccumR :: forall a b s f. Traversable f => (s -> a -> Accum s b) -> s -> f a -> Accum s (f b)
```

Fold a data structure from the right, keeping all intermediate results
instead of only the final result.

Unlike `scanr`, `mapAccumR` allows the type of accumulator to differ
from the element type of the final data structure.

#### `mapAccumL`

``` purescript
mapAccumL :: forall a b s f. Traversable f => (s -> a -> Accum s b) -> s -> f a -> Accum s (f b)
```

Fold a data structure from the left, keeping all intermediate results
instead of only the final result.

Unlike `scanl`, `mapAccumL` allows the type of accumulator to differ
from the element type of the final data structure.

#### `intercalate`

``` purescript
intercalate :: forall f m. (Foldable f, Monoid m) => m -> f m -> m
```

Fold a data structure, accumulating values in some `Monoid`,
combining adjacent elements using the specified separator.

#### `for_`

``` purescript
for_ :: forall a b f m. (Applicative m, Foldable f) => f a -> (a -> m b) -> m Unit
```

A version of `traverse_` with its arguments flipped.

This can be useful when running an action written using do notation
for every element in a data structure:

For example:

```purescript
for_ [1, 2, 3] \n -> do
  print n
  trace "squared is"
  print (n * n)
```

#### `for`

``` purescript
for :: forall a b m t. (Applicative m, Traversable t) => t a -> (a -> m b) -> m (t b)
```

A version of `traverse` with its arguments flipped.


This can be useful when running an action written using do notation
for every element in a data structure:

For example:

```purescript
for [1, 2, 3] \n -> do
  print n
  return (n * n)
```

#### `foldrDefault`

``` purescript
foldrDefault :: forall f a b. Foldable f => (a -> b -> b) -> b -> f a -> b
```

A default implementation of `foldr` using `foldMap`.

Note: when defining a `Foldable` instance, this function is unsafe to use
in combination with `foldMapDefaultR`.

#### `foldlDefault`

``` purescript
foldlDefault :: forall f a b. Foldable f => (b -> a -> b) -> b -> f a -> b
```

A default implementation of `foldl` using `foldMap`.

Note: when defining a `Foldable` instance, this function is unsafe to use
in combination with `foldMapDefaultL`.

#### `foldMapDefaultR`

``` purescript
foldMapDefaultR :: forall f a m. (Foldable f, Monoid m) => (a -> m) -> f a -> m
```

A default implementation of `foldMap` using `foldr`.

Note: when defining a `Foldable` instance, this function is unsafe to use
in combination with `foldrDefault`.

#### `foldMapDefaultL`

``` purescript
foldMapDefaultL :: forall f a m. (Foldable f, Monoid m) => (a -> m) -> f a -> m
```

A default implementation of `foldMap` using `foldl`.

Note: when defining a `Foldable` instance, this function is unsafe to use
in combination with `foldlDefault`.

#### `fold`

``` purescript
fold :: forall f m. (Foldable f, Monoid m) => f m -> m
```

Fold a data structure, accumulating values in some `Monoid`.

#### `find`

``` purescript
find :: forall a f. Foldable f => (a -> Boolean) -> f a -> Maybe a
```

Try to find an element in a data structure which satisfies a predicate.

#### `elem`

``` purescript
elem :: forall a f. (Foldable f, Eq a) => a -> f a -> Boolean
```

Test whether a value is an element of a data structure.

#### `any`

``` purescript
any :: forall a b f. (Foldable f, BooleanAlgebra b) => (a -> b) -> f a -> b
```

`any f` is the same as `or <<< map f`; map a function over the structure,
and then get the disjunction of the results.

#### `and`

``` purescript
and :: forall a f. (Foldable f, BooleanAlgebra a) => f a -> a
```

The conjunction of all the values in a data structure. When specialized
to `Boolean`, this function will test whether all of the values in a data
structure are `true`.

#### `all`

``` purescript
all :: forall a b f. (Foldable f, BooleanAlgebra b) => (a -> b) -> f a -> b
```

`all f` is the same as `and <<< map f`; map a function over the structure,
and then get the conjunction of the results.

### Re-exported from Data.Tuple:

#### `Tuple`

``` purescript
data Tuple a b
  = Tuple a b
```

A simple product type for wrapping a pair of component values.

##### Instances
``` purescript
(Show a, Show b) => Show (Tuple a b)
(Eq a, Eq b) => Eq (Tuple a b)
(Ord a, Ord b) => Ord (Tuple a b)
(Bounded a, Bounded b) => Bounded (Tuple a b)
Semigroupoid Tuple
(Semigroup a, Semigroup b) => Semigroup (Tuple a b)
(Monoid a, Monoid b) => Monoid (Tuple a b)
(Semiring a, Semiring b) => Semiring (Tuple a b)
(Ring a, Ring b) => Ring (Tuple a b)
(CommutativeRing a, CommutativeRing b) => CommutativeRing (Tuple a b)
(HeytingAlgebra a, HeytingAlgebra b) => HeytingAlgebra (Tuple a b)
(BooleanAlgebra a, BooleanAlgebra b) => BooleanAlgebra (Tuple a b)
Functor (Tuple a)
Invariant (Tuple a)
Bifunctor Tuple
(Semigroup a) => Apply (Tuple a)
Biapply Tuple
(Monoid a) => Applicative (Tuple a)
Biapplicative Tuple
(Semigroup a) => Bind (Tuple a)
(Monoid a) => Monad (Tuple a)
Extend (Tuple a)
Comonad (Tuple a)
(Lazy a, Lazy b) => Lazy (Tuple a b)
Foldable (Tuple a)
Bifoldable Tuple
Traversable (Tuple a)
Bitraversable Tuple
```

#### `uncurry`

``` purescript
uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c
```

Turn a function of two arguments into a function that expects a tuple.

#### `swap`

``` purescript
swap :: forall a b. Tuple a b -> Tuple b a
```

Exchange the first and second components of a tuple.

#### `snd`

``` purescript
snd :: forall a b. Tuple a b -> b
```

Returns the second component of a tuple.

#### `lookup`

``` purescript
lookup :: forall a b f. (Foldable f, Eq a) => a -> f (Tuple a b) -> Maybe b
```

Lookup a value in a data structure of `Tuple`s, generalizing association lists.

#### `fst`

``` purescript
fst :: forall a b. Tuple a b -> a
```

Returns the first component of a tuple.

#### `curry`

``` purescript
curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c
```

Turn a function that expects a tuple into a function of two arguments.

### Re-exported from Data.Unfoldable:

#### `Unfoldable`

``` purescript
class Unfoldable t where
  unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> t a
```

This class identifies data structures which can be _unfolded_,
generalizing `unfoldr` on arrays.

The generating function `f` in `unfoldr f` in understood as follows:

- If `f b` is `Nothing`, then `unfoldr f b` should be empty.
- If `f b` is `Just (Tuple a b1)`, then `unfoldr f b` should consist of `a`
  appended to the result of `unfoldr f b1`.

##### Instances
``` purescript
Unfoldable Array
```

#### `singleton`

``` purescript
singleton :: forall f a. Unfoldable f => a -> f a
```

Contain a single value.
For example:

~~~ purescript
singleton "foo" == ["foo"] :: Array String
~~~

#### `replicateA`

``` purescript
replicateA :: forall m f a. (Applicative m, Unfoldable f, Traversable f) => Int -> m a -> m (f a)
```

Perform an Applicative action `n` times, and accumulate all the results.

#### `none`

``` purescript
none :: forall f a. Unfoldable f => f a
```

The container with no elements - unfolded with zero iterations.
For example:

~~~ purescript
none == [] :: forall a. Array a
~~~

### Re-exported from Prelude:

#### `Void`

``` purescript
newtype Void
```

##### Instances
``` purescript
Show Void
```

#### `Unit`

``` purescript
data Unit :: *
```

The `Unit` type has a single inhabitant, called `unit`. It represents
values with no computational content.

`Unit` is often used, wrapped in a monadic type constructor, as the
return type of a computation where only
the _effects_ are important.

##### Instances
``` purescript
Show Unit
```

#### `Ordering`

``` purescript
data Ordering
  = LT
  | GT
  | EQ
```

The `Ordering` data type represents the three possible outcomes of
comparing two values:

`LT` - The first value is _less than_ the second.
`GT` - The first value is _greater than_ the second.
`EQ` - The first value is _equal to_ the second.

##### Instances
``` purescript
Eq Ordering
Semigroup Ordering
Show Ordering
```

#### `Applicative`

``` purescript
class (Apply f) <= Applicative f where
  pure :: forall a. a -> f a
```

The `Applicative` type class extends the [`Apply`](#apply) type class
with a `pure` function, which can be used to create values of type `f a`
from values of type `a`.

Where [`Apply`](#apply) provides the ability to lift functions of two or
more arguments to functions whose arguments are wrapped using `f`, and
[`Functor`](#functor) provides the ability to lift functions of one
argument, `pure` can be seen as the function which lifts functions of
_zero_ arguments. That is, `Applicative` functors support a lifting
operation for any number of function arguments.

Instances must satisfy the following laws in addition to the `Apply`
laws:

- Identity: `(pure id) <*> v = v`
- Composition: `(pure <<<) <*> f <*> g <*> h = f <*> (g <*> h)`
- Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
- Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`

##### Instances
``` purescript
Applicative (Function r)
Applicative Array
```

#### `Apply`

``` purescript
class (Functor f) <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b
```

The `Apply` class provides the `(<*>)` which is used to apply a function
to an argument under a type constructor.

`Apply` can be used to lift functions of two or more arguments to work on
values wrapped with the type constructor `f`. It might also be understood
in terms of the `lift2` function:

```purescript
lift2 :: forall f a b c. Apply f => (a -> b -> c) -> f a -> f b -> f c
lift2 f a b = f <$> a <*> b
```

`(<*>)` is recovered from `lift2` as `lift2 ($)`. That is, `(<*>)` lifts
the function application operator `($)` to arguments wrapped with the
type constructor `f`.

Instances must satisfy the following law in addition to the `Functor`
laws:

- Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`

Formally, `Apply` represents a strong lax semi-monoidal endofunctor.

##### Instances
``` purescript
Apply (Function r)
Apply Array
```

#### `Bind`

``` purescript
class (Apply m) <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b
```

The `Bind` type class extends the [`Apply`](#apply) type class with a
"bind" operation `(>>=)` which composes computations in sequence, using
the return value of one computation to determine the next computation.

The `>>=` operator can also be expressed using `do` notation, as follows:

```purescript
x >>= f = do y <- x
             f y
```

where the function argument of `f` is given the name `y`.

Instances must satisfy the following law in addition to the `Apply`
laws:

- Associativity: `(x >>= f) >>= g = x >>= (\k -> f k >>= g)`

Associativity tells us that we can regroup operations which use `do`
notation so that we can unambiguously write, for example:

```purescript
do x <- m1
   y <- m2 x
   m3 x y
```

##### Instances
``` purescript
Bind (Function r)
Bind Array
```

#### `BooleanAlgebra`

``` purescript
class (HeytingAlgebra a) <= BooleanAlgebra a
```

The `BooleanAlgebra` type class represents types that behave like boolean
values.

Instances should satisfy the following laws in addition to the
`HeytingAlgebra` law:

- Excluded middle:
  - `a || not a = tt`

##### Instances
``` purescript
BooleanAlgebra Boolean
BooleanAlgebra Unit
```

#### `Bounded`

``` purescript
class (Ord a) <= Bounded a where
  top :: a
  bottom :: a
```

The `Bounded` type class represents totally ordered types that have an
upper and lower boundary.

Instances should satisfy the following law in addition to the `Ord` laws:

- Bounded: `bottom <= a <= top`

##### Instances
``` purescript
Bounded Boolean
Bounded Int
Bounded Char
Bounded Ordering
Bounded Unit
```

#### `Category`

``` purescript
class (Semigroupoid a) <= Category a where
  id :: forall t. a t t
```

`Category`s consist of objects and composable morphisms between them, and
as such are [`Semigroupoids`](#semigroupoid), but unlike `semigroupoids`
must have an identity element.

Instances must satisfy the following law in addition to the
`Semigroupoid` law:

- Identity: `id <<< p = p <<< id = p`

##### Instances
``` purescript
Category Function
```

#### `CommutativeRing`

``` purescript
class (Ring a) <= CommutativeRing a
```

The `CommutativeRing` class is for rings where multiplication is
commutative.

Instances must satisfy the following law in addition to the `Ring`
laws:

- Commutative multiplication: `a * b = b * a`

##### Instances
``` purescript
CommutativeRing Int
CommutativeRing Number
CommutativeRing Unit
```

#### `Eq`

``` purescript
class Eq a where
  eq :: a -> a -> Boolean
```

The `Eq` type class represents types which support decidable equality.

`Eq` instances should satisfy the following laws:

- Reflexivity: `x == x = true`
- Symmetry: `x == y = y == x`
- Transitivity: if `x == y` and `y == z` then `x == z`

##### Instances
``` purescript
Eq Boolean
Eq Int
Eq Number
Eq Char
Eq String
Eq Unit
Eq Void
(Eq a) => Eq (Array a)
```

#### `EuclideanRing`

``` purescript
class (CommutativeRing a) <= EuclideanRing a where
  degree :: a -> Int
  div :: a -> a -> a
  mod :: a -> a -> a
```

The `EuclideanRing` class is for commutative rings that support division.

Instances must satisfy the following law in addition to the `Ring`
laws:

- Integral domain: `a /= 0` and `b /= 0` implies `a * b /= 0`
- Multiplicative Euclidean function: ``a = (a / b) * b + (a `mod` b)``
  where `degree a > 0` and `degree a <= degree (a * b)`

##### Instances
``` purescript
EuclideanRing Int
EuclideanRing Number
EuclideanRing Unit
```

#### `Field`

``` purescript
class (EuclideanRing a) <= Field a
```

The `Field` class is for types that are commutative fields.

Instances must satisfy the following law in addition to the
`EuclideanRing` laws:

- Non-zero multiplicative inverse: ``a `mod` b = 0` for all `a` and `b`

##### Instances
``` purescript
Field Number
Field Unit
```

#### `Functor`

``` purescript
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
```

A `Functor` is a type constructor which supports a mapping operation
`(<$>)`.

`(<$>)` can be used to turn functions `a -> b` into functions
`f a -> f b` whose argument and return types use the type constructor `f`
to represent some computational context.

Instances must satisfy the following laws:

- Identity: `(<$>) id = id`
- Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`

##### Instances
``` purescript
Functor (Function r)
Functor Array
```

#### `HeytingAlgebra`

``` purescript
class HeytingAlgebra a where
  conj :: a -> a -> a
  disj :: a -> a -> a
  not :: a -> a
```

The `HeytingAlgebra` type class represents types are bounded lattices with
an implication operator such that the following laws hold:

- Associativity:
  - `a || (b || c) = (a || b) || c`
  - `a && (b && c) = (a && b) && c`
- Commutativity:
  - `a || b = b || a`
  - `a && b = b && a`
- Absorption:
  - `a || (a && b) = a`
  - `a && (a || b) = a`
- Idempotent:
  - `a || a = a`
  - `a && a = a`
- Identity:
  - `a || ff = a`
  - `a && tt = a`
- Implication:
  - ``a `implies` a = tt``
  - ``a && (a `implies` b) = a && b``
  - ``b && (a `implies` b) = b``
  - ``a `implies` (b && c) = (a `implies` b) && (a `implies` c)``
- Complemented:
  - ``not a = a `implies` ff``

##### Instances
``` purescript
HeytingAlgebra Boolean
HeytingAlgebra Unit
(HeytingAlgebra b) => HeytingAlgebra (a -> b)
```

#### `Monad`

``` purescript
class (Applicative m, Bind m) <= Monad m
```

The `Monad` type class combines the operations of the `Bind` and
`Applicative` type classes. Therefore, `Monad` instances represent type
constructors which support sequential composition, and also lifting of
functions of arbitrary arity.

Instances must satisfy the following laws in addition to the
`Applicative` and `Bind` laws:

- Left Identity: `pure x >>= f = f x`
- Right Identity: `x >>= pure = x`

##### Instances
``` purescript
Monad (Function r)
Monad Array
```

#### `Ord`

``` purescript
class (Eq a) <= Ord a where
  compare :: a -> a -> Ordering
```

The `Ord` type class represents types which support comparisons with a
_total order_.

`Ord` instances should satisfy the laws of total orderings:

- Reflexivity: `a <= a`
- Antisymmetry: if `a <= b` and `b <= a` then `a = b`
- Transitivity: if `a <= b` and `b <= c` then `a <= c`

##### Instances
``` purescript
Ord Boolean
Ord Int
Ord Number
Ord String
Ord Char
Ord Unit
Ord Void
(Ord a) => Ord (Array a)
Ord Ordering
```

#### `Ring`

``` purescript
class (Semiring a) <= Ring a where
  sub :: a -> a -> a
```

The `Ring` class is for types that support addition, multiplication,
and subtraction operations.

Instances must satisfy the following law in addition to the `Semiring`
laws:

- Additive inverse: `a - a = (zero - a) + a = zero`

##### Instances
``` purescript
Ring Int
Ring Number
Ring Unit
```

#### `Semigroup`

``` purescript
class Semigroup a where
  append :: a -> a -> a
```

The `Semigroup` type class identifies an associative operation on a type.

Instances are required to satisfy the following law:

- Associativity: `(x <> y) <> z = x <> (y <> z)`

One example of a `Semigroup` is `String`, with `(<>)` defined as string
concatenation.

##### Instances
``` purescript
Semigroup String
Semigroup Unit
Semigroup Void
(Semigroup s') => Semigroup (s -> s')
Semigroup (Array a)
```

#### `Semigroupoid`

``` purescript
class Semigroupoid a where
  compose :: forall b c d. a c d -> a b c -> a b d
```

A `Semigroupoid` is similar to a [`Category`](#category) but does not
require an identity element `id`, just composable morphisms.

`Semigroupoid`s must satisfy the following law:

- Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`

One example of a `Semigroupoid` is the function type constructor `(->)`,
with `(<<<)` defined as function composition.

##### Instances
``` purescript
Semigroupoid Function
```

#### `Semiring`

``` purescript
class Semiring a where
  add :: a -> a -> a
  zero :: a
  mul :: a -> a -> a
  one :: a
```

The `Semiring` class is for types that support an addition and
multiplication operation.

Instances must satisfy the following laws:

- Commutative monoid under addition:
  - Associativity: `(a + b) + c = a + (b + c)`
  - Identity: `zero + a = a + zero = a`
  - Commutative: `a + b = b + a`
- Monoid under multiplication:
  - Associativity: `(a * b) * c = a * (b * c)`
  - Identity: `one * a = a * one = a`
- Multiplication distributes over addition:
  - Left distributivity: `a * (b + c) = (a * b) + (a * c)`
  - Right distributivity: `(a + b) * c = (a * c) + (b * c)`
- Annihiliation: `zero * a = a * zero = zero`

##### Instances
``` purescript
Semiring Int
Semiring Number
Semiring Unit
```

#### `Show`

``` purescript
class Show a where
  show :: a -> String
```

The `Show` type class represents those types which can be converted into
a human-readable `String` representation.

While not required, it is recommended that for any expression `x`, the
string `show x` be executable PureScript code which evaluates to the same
value as the expression `x`.

##### Instances
``` purescript
Show Boolean
Show Int
Show Number
Show Char
Show String
(Show a) => Show (Array a)
```

#### `when`

``` purescript
when :: forall m. Applicative m => Boolean -> m Unit -> m Unit
```

Perform a applicative action when a condition is true.

#### `void`

``` purescript
void :: forall f a. Functor f => f a -> f Unit
```

The `void` function is used to ignore the type wrapped by a
[`Functor`](#functor), replacing it with `Unit` and keeping only the type
information provided by the type constructor itself.

`void` is often useful when using `do` notation to change the return type
of a monadic computation:

```purescript
main = forE 1 10 \n -> void do
  print n
  print (n * n)
```

#### `unless`

``` purescript
unless :: forall m. Applicative m => Boolean -> m Unit -> m Unit
```

Perform a applicative action unless a condition is true.

#### `unit`

``` purescript
unit :: Unit
```

`unit` is the sole inhabitant of the `Unit` type.

#### `otherwise`

``` purescript
otherwise :: Boolean
```

An alias for `true`, which can be useful in guard clauses:

```purescript
max x y | x >= y    = x
        | otherwise = y
```

#### `notEq`

``` purescript
notEq :: forall a. Eq a => a -> a -> Boolean
```

`notEq` tests whether one value is _not equal_ to another. Shorthand for
`not (eq x y)`.

#### `negate`

``` purescript
negate :: forall a. Ring a => a -> a
```

`negate x` can be used as a shorthand for `zero - x`.

#### `min`

``` purescript
min :: forall a. Ord a => a -> a -> a
```

Take the minimum of two values. If they are considered equal, the first
argument is chosen.

#### `max`

``` purescript
max :: forall a. Ord a => a -> a -> a
```

Take the maximum of two values. If they are considered equal, the first
argument is chosen.

#### `liftM1`

``` purescript
liftM1 :: forall m a b. Monad m => (a -> b) -> m a -> m b
```

`liftM1` provides a default implementation of `(<$>)` for any
[`Monad`](#monad), without using `(<$>)` as provided by the
[`Functor`](#functor)-[`Monad`](#monad) superclass relationship.

`liftM1` can therefore be used to write [`Functor`](#functor) instances
as follows:

```purescript
instance functorF :: Functor F where
  map = liftM1
```

#### `liftA1`

``` purescript
liftA1 :: forall f a b. Applicative f => (a -> b) -> f a -> f b
```

`liftA1` provides a default implementation of `(<$>)` for any
[`Applicative`](#applicative) functor, without using `(<$>)` as provided
by the [`Functor`](#functor)-[`Applicative`](#applicative) superclass
relationship.

`liftA1` can therefore be used to write [`Functor`](#functor) instances
as follows:

```purescript
instance functorF :: Functor F where
  map = liftA1
```

#### `join`

``` purescript
join :: forall a m. Bind m => m (m a) -> m a
```

Collapse two applications of a monadic type constructor into one.

#### `ifM`

``` purescript
ifM :: forall a m. Bind m => m Boolean -> m a -> m a -> m a
```

Execute a monadic action if a condition holds.

For example:

```purescript
main = ifM ((< 0.5) <$> random)
         (trace "Heads")
         (trace "Tails")
```

#### `flip`

``` purescript
flip :: forall a b c. (a -> b -> c) -> b -> a -> c
```

Flips the order of the arguments to a function of two arguments.

```purescript
flip const 1 2 = const 2 1 = 2
```

#### `const`

``` purescript
const :: forall a b. a -> b -> a
```

Returns its first argument and ignores its second.

```purescript
const 1 "hello" = 1
```

#### `comparing`

``` purescript
comparing :: forall a b. Ord b => (a -> b) -> (a -> a -> Ordering)
```

Compares two values by mapping them to a type with an `Ord` instance.

#### `clamp`

``` purescript
clamp :: forall a. Ord a => a -> a -> a -> a
```

Clamp a value between a minimum and a maximum. For example:

``` purescript
let f = clamp 0 10
f (-5) == 0
f 5    == 5
f 15   == 10
```

#### `between`

``` purescript
between :: forall a. Ord a => a -> a -> a -> Boolean
```

Test whether a value is between a minimum and a maximum (inclusive).
For example:

``` purescript
let f = between 0 10
f 0    == true
f (-5) == false
f 5    == true
f 10   == true
f 15   == false
```

#### `ap`

``` purescript
ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
```

`ap` provides a default implementation of `(<*>)` for any
[`Monad`](#monad), without using `(<*>)` as provided by the
[`Apply`](#apply)-[`Monad`](#monad) superclass relationship.

`ap` can therefore be used to write [`Apply`](#apply) instances as
follows:

```purescript
instance applyF :: Apply F where
  apply = ap
```

#### `absurd`

``` purescript
absurd :: forall a. Void -> a
```

#### `(||)`

``` purescript
infixr 2 Data.HeytingAlgebra.disj as ||
```

#### `(>>>)`

``` purescript
infixr 9 Control.Semigroupoid.composeFlipped as >>>
```

#### `(>>=)`

``` purescript
infixl 1 Control.Bind.bind as >>=
```

#### `(>=>)`

``` purescript
infixr 1 Control.Bind.composeKleisli as >=>
```

#### `(>=)`

``` purescript
infixl 4 Data.Ord.greaterThanOrEq as >=
```

#### `(>)`

``` purescript
infixl 4 Data.Ord.greaterThan as >
```

#### `(==)`

``` purescript
infix 4 Data.Eq.eq as ==
```

#### `(=<<)`

``` purescript
infixl 1 Control.Bind.bindFlipped as =<<
```

#### `(<>)`

``` purescript
infixr 5 Data.Semigroup.append as <>
```

#### `(<=<)`

``` purescript
infixr 1 Control.Bind.composeKleisliFlipped as <=<
```

#### `(<=)`

``` purescript
infixl 4 Data.Ord.lessThanOrEq as <=
```

#### `(<<<)`

``` purescript
infixr 9 Control.Semigroupoid.compose as <<<
```

#### `(<*>)`

``` purescript
infixl 4 Control.Apply.apply as <*>
```

#### `(<*)`

``` purescript
infixl 4 Control.Apply.applyFirst as <*
```

#### `(<$>)`

``` purescript
infixl 4 Data.Functor.map as <$>
```

#### `(<$)`

``` purescript
infixl 4 Data.Functor.voidRight as <$
```

#### `(<#>)`

``` purescript
infixl 1 Data.Functor.mapFlipped as <#>
```

#### `(<)`

``` purescript
infixl 4 Data.Ord.lessThan as <
```

#### `(/=)`

``` purescript
infix 4 Data.Eq.notEq as /=
```

#### `(/)`

``` purescript
infixl 7 Data.EuclideanRing.div as /
```

#### `(-)`

``` purescript
infixl 6 Data.Ring.sub as -
```

#### `(+)`

``` purescript
infixl 6 Data.Semiring.add as +
```

#### `(*>)`

``` purescript
infixl 4 Control.Apply.applySecond as *>
```

#### `(*)`

``` purescript
infixl 7 Data.Semiring.mul as *
```

#### `(&&)`

``` purescript
infixr 3 Data.HeytingAlgebra.conj as &&
```

#### `($>)`

``` purescript
infixl 4 Data.Functor.voidLeft as $>
```

#### `($)`

``` purescript
infixr 0 Data.Function.apply as $
```

Applies a function to an argument: the reverse of `(#)`.

```purescript
length $ groupBy productCategory $ filter isInStock $ products
```

is equivalent to:

```purescript
length (groupBy productCategory (filter isInStock products))
```

Or another alternative equivalent, applying chain of composed functions to
a value:

```purescript
length <<< groupBy productCategory <<< filter isInStock $ products
```

#### `(#)`

``` purescript
infixl 1 Data.Function.applyFlipped as #
```

Applies an argument to a function: the reverse of `($)`.

```purescript
products # filter isInStock # groupBy productCategory # length
```

is equivalent to:

```purescript
length (groupBy productCategory (filter isInStock products))
```

Or another alternative equivalent, applying a value to a chain of composed
functions:

```purescript
products # filter isInStock >>> groupBy productCategory >>> length
```

#### `type (~>)`

``` purescript
infixr 4 type Data.NaturalTransformation.NaturalTransformation as ype (~>
```

