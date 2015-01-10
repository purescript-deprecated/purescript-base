# Module Documentation

## Module Math

### Types


    type Radians = Number


### Values


    abs :: Number -> Number


    acos :: Number -> Radians


    asin :: Number -> Radians


    atan :: Number -> Radians


    atan2 :: Number -> Number -> Radians


    ceil :: Number -> Number


    cos :: Radians -> Number


    e :: Number


    exp :: Number -> Number


    floor :: Number -> Number


    ln10 :: Number


    ln2 :: Number


    log :: Number -> Number


    log10e :: Number


    log2e :: Number


    max :: Number -> Number -> Number


    min :: Number -> Number -> Number


    pi :: Number


    pow :: Number -> Number -> Number


    round :: Number -> Number


    sin :: Radians -> Number


    sqrt :: Number -> Number


    sqrt1_2 :: Number


    sqrt2 :: Number


    tan :: Radians -> Number


## Module Main

## Module Main

### Values


    testReader :: Reader String String


## Module Main

### Values


    incState :: forall eff a. State Number Unit


    testState :: forall eff a. State Number String


## Module Main

### Values


    testWriter :: Writer String Number


## Module Main

### Values


    collatz :: Number -> [Number]


## Module Data.Array

### Type Class Instances


    instance altArray :: Alt Prim.Array


    instance alternativeArray :: Alternative Prim.Array


    instance applicativeArray :: Applicative Prim.Array


    instance applyArray :: Apply Prim.Array


    instance bindArray :: Bind Prim.Array


    instance functorArray :: Functor Prim.Array


    instance monadArray :: Monad Prim.Array


    instance monadPlusArray :: MonadPlus Prim.Array


    instance plusArray :: Plus Prim.Array


    instance semigroupArray :: Semigroup [a]


### Values


    (!!) :: forall a. [a] -> Number -> Maybe a


    (..) :: Number -> Number -> [Number]


    (\\) :: forall a. (Eq a) => [a] -> [a] -> [a]


    append :: forall a. [a] -> [a] -> [a]


    catMaybes :: forall a. [Maybe a] -> [a]


    concat :: forall a. [[a]] -> [a]


    concatMap :: forall a b. (a -> [b]) -> [a] -> [b]


    delete :: forall a. (Eq a) => a -> [a] -> [a]


    deleteAt :: forall a. Number -> Number -> [a] -> [a]


    deleteBy :: forall a. (a -> a -> Boolean) -> a -> [a] -> [a]


    drop :: forall a. Number -> [a] -> [a]


    elemIndex :: forall a. (Eq a) => a -> [a] -> Number


    elemLastIndex :: forall a. (Eq a) => a -> [a] -> Number


    filter :: forall a. (a -> Boolean) -> [a] -> [a]


    findIndex :: forall a. (a -> Boolean) -> [a] -> Number


    findLastIndex :: forall a. (a -> Boolean) -> [a] -> Number


    group :: forall a. (Eq a) => [a] -> [[a]]

     | Performs a sorting first.

    group' :: forall a. (Ord a) => [a] -> [[a]]


    groupBy :: forall a. (a -> a -> Boolean) -> [a] -> [[a]]


    head :: forall a. [a] -> Maybe a


    init :: forall a. [a] -> Maybe [a]


    insertAt :: forall a. Number -> a -> [a] -> [a]


    intersect :: forall a. (Eq a) => [a] -> [a] -> [a]


    intersectBy :: forall a. (a -> a -> Boolean) -> [a] -> [a] -> [a]


    last :: forall a. [a] -> Maybe a


    length :: forall a. [a] -> Number


    map :: forall a b. (a -> b) -> [a] -> [b]


    mapMaybe :: forall a b. (a -> Maybe b) -> [a] -> [b]


    nub :: forall a. (Eq a) => [a] -> [a]


    nubBy :: forall a. (a -> a -> Boolean) -> [a] -> [a]


    null :: forall a. [a] -> Boolean


    range :: Number -> Number -> [Number]


    reverse :: forall a. [a] -> [a]


    singleton :: forall a. a -> [a]


    snoc :: forall a. [a] -> a -> [a]


    sort :: forall a. (Ord a) => [a] -> [a]


    sortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]


    span :: forall a. (a -> Boolean) -> [a] -> { rest :: [a], init :: [a] }


    tail :: forall a. [a] -> Maybe [a]


    take :: forall a. Number -> [a] -> [a]


    updateAt :: forall a. Number -> a -> [a] -> [a]


    zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]


## Module Control.Alt

### Type Classes


    class (Functor f) <= Alt f where

      (<|>) :: forall a. f a -> f a -> f a


## Module Control.Alternative

### Type Classes


    class (Applicative f, Plus f) <= Alternative f where


### Values


    many :: forall f a. (Alternative f, Lazy1 f) => f a -> f [a]


    some :: forall f a. (Alternative f, Lazy1 f) => f a -> f [a]


## Module Control.Apply

### Values


    (*>) :: forall a b f. (Apply f) => f a -> f b -> f b


    (<*) :: forall a b f. (Apply f) => f a -> f b -> f a


    forever :: forall a b f. (Apply f) => f a -> f b


    lift2 :: forall a b c f. (Apply f) => (a -> b -> c) -> f a -> f b -> f c


    lift3 :: forall a b c d f. (Apply f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d


    lift4 :: forall a b c d e f. (Apply f) => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e


    lift5 :: forall a b c d e f g. (Apply f) => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g


## Module Control.Bind

### Values


    (<=<) :: forall a b c m. (Bind m) => (b -> m c) -> (a -> m b) -> a -> m c


    (=<<) :: forall a b m. (Bind m) => (a -> m b) -> m a -> m b


    (>=>) :: forall a b c m. (Bind m) => (a -> m b) -> (b -> m c) -> a -> m c


    ifM :: forall a m. (Bind m) => m Boolean -> m a -> m a -> m a


    join :: forall a m. (Bind m) => m (m a) -> m a


## Module Control.Comonad

### Type Classes


    class (Extend w) <= Comonad w where

      extract :: forall a. w a -> a


## Module Control.Extend

### Type Classes


    class (Functor w) <= Extend w where

      (<<=) :: forall b a. (w a -> b) -> w a -> w b


### Type Class Instances


    instance extendArr :: (Semigroup w) => Extend (Prim.Function w)


### Values


    (=<=) :: forall b a w c. (Extend w) => (w b -> c) -> (w a -> b) -> w a -> c


    (=>=) :: forall b a w c. (Extend w) => (w a -> b) -> (w b -> c) -> w a -> c


    (=>>) :: forall b a w. (Extend w) => w a -> (w a -> b) -> w b


    duplicate :: forall a w. (Extend w) => w a -> w (w a)


## Module Control.Lazy

### Type Classes


    class Lazy l where

      defer :: (Unit -> l) -> l


    class Lazy1 l where

      defer1 :: forall a. (Unit -> l a) -> l a


    class Lazy2 l where

      defer2 :: forall a b. (Unit -> l a b) -> l a b


### Values


    fix :: forall l a. (Lazy l) => (l -> l) -> l


    fix1 :: forall l a. (Lazy1 l) => (l a -> l a) -> l a


    fix2 :: forall l a b. (Lazy2 l) => (l a b -> l a b) -> l a b


## Module Control.Monad

### Values


    foldM :: forall m a b. (Monad m) => (a -> b -> m a) -> a -> [b] -> m a


    replicateM :: forall m a. (Monad m) => Number -> m a -> m [a]


    unless :: forall m. (Monad m) => Boolean -> m Unit -> m Unit


    when :: forall m. (Monad m) => Boolean -> m Unit -> m Unit


## Module Control.MonadPlus

### Type Classes


    class (Monad m, Alternative m) <= MonadPlus m where


### Values


    guard :: forall m. (MonadPlus m) => Boolean -> m Unit


## Module Control.Plus

### Type Classes


    class (Alt f) <= Plus f where

      empty :: forall a. f a


## Module Data.Either

### Types


    data Either a b where
      Left :: a -> Either a b
      Right :: b -> Either a b


### Type Class Instances


    instance altEither :: Alt (Either e)


    instance applicativeEither :: Applicative (Either e)


    instance applyEither :: Apply (Either e)


    instance bindEither :: Bind (Either e)


    instance eqEither :: (Eq a, Eq b) => Eq (Either a b)


    instance functorEither :: Functor (Either a)


    instance monadEither :: Monad (Either e)


    instance ordEither :: (Ord a, Ord b) => Ord (Either a b)


    instance showEither :: (Show a, Show b) => Show (Either a b)


### Values


    either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c


    isLeft :: forall a b. Either a b -> Boolean


    isRight :: forall a b. Either a b -> Boolean


## Module Data.Enum

### Types


    newtype Cardinality a where
      Cardinality :: Number -> Cardinality a


### Type Classes

     | Type class for enumerations. This should not be considered a part of a
     | numeric hierarchy, ala Haskell. Rather, this is a type class for small,
     | ordered sum types with statically-determined cardinality and the ability 
     | to easily compute successor and predecessor elements. e.g. DayOfWeek, etc.
     |
     | Laws:
     |   succ firstEnum >>= succ >>= succ ... succ [cardinality - 1 times] == lastEnum
     |   pred lastEnum  >>= pred >>= pred ... pred [cardinality - 1 times] == firstEnum
     |   
     |   e1 `compare` e2 == fromEnum e1 `compare` fromEnum e2
     |   
     |   for all a > firstEnum: pred a >>= succ == Just a
     |   for all a < lastEnum:  succ a >>= pred == Just a
     |   
     |   pred >=> succ >=> pred = pred
     |   succ >=> pred >=> succ = succ
     |   
     |   toEnum (fromEnum a) = Just a
     |   
     |   for all a > firstEnum: fromEnum <$> pred a = Just (fromEnum a - 1)
     |   for all a < lastEnum:  fromEnum <$> succ a = Just (fromEnum a + 1)

    class (Ord a) <= Enum a where

      cardinality :: Cardinality a

      firstEnum :: a

      lastEnum :: a

      succ :: a -> Maybe a

      pred :: a -> Maybe a

      toEnum :: Number -> Maybe a

      fromEnum :: a -> Number


### Type Class Instances


    instance enumBoolean :: Enum Boolean

     starting value/state.
     | Instances

    instance enumChar :: Enum Char


    instance enumEither :: (Enum a, Enum b) => Enum (Either a b)


    instance enumMaybe :: (Enum a) => Enum (Maybe a)


    instance enumTuple :: (Enum a, Enum b) => Enum (Tuple a b)


### Values

     | Runs in O(n) where n is (fromEnum a)
     | defaultFromEnum pred = fromEnum

    defaultFromEnum :: forall a. (a -> Maybe a) -> a -> Number

     | defaultPred toEnum fromEnum = pred

    defaultPred :: forall a. (Number -> Maybe a) -> (a -> Number) -> a -> Maybe a

     | defaultSucc toEnum fromEnum = succ

    defaultSucc :: forall a. (Number -> Maybe a) -> (a -> Number) -> a -> Maybe a

     | Runs in O(n) where n is (fromEnum a)
     | defaultToEnum succ firstEnum = toEnum

    defaultToEnum :: forall a. (a -> Maybe a) -> a -> Number -> Maybe a

     [a,b..c]
     Correctness for using fromJust is the same as for enumFromTo.

    enumFromThenTo :: forall a. (Enum a) => a -> a -> a -> [a]

     Property: fromEnum a = a', fromEnum b = b' => forall e', a' <= e' <= b': Exists e: toEnum e' = Just e
     Following from the propery of intFromTo, We are sure all elements in intFromTo (fromEnum a) (fromEnum b) are Justs.

    enumFromTo :: forall a. (Enum a) => a -> a -> [a]

     Property: forall e in intFromTo a b: a <= e <= b
     intFromTo :: Int -> Int -> List Int

    intFromTo :: Number -> Number -> [Number]

     Property: forall e in intStepFromTo step a b: a <= e <= b
     intStepFromTo :: Int -> Int -> Int -> List Int

    intStepFromTo :: Number -> Number -> Number -> [Number]


    runCardinality :: forall a. Cardinality a -> Number


## Module Data.Exists

### Types


    data Exists :: (* -> *) -> *


### Values


    mkExists :: forall f a. f a -> Exists f


    runExists :: forall f r. (forall a. f a -> r) -> Exists f -> r


## Module Data.Foldable

### Type Classes


    class Foldable f where

      foldr :: forall a b. (a -> b -> b) -> b -> f a -> b

      foldl :: forall a b. (b -> a -> b) -> b -> f a -> b

      foldMap :: forall a m. (Monoid m) => (a -> m) -> f a -> m


### Type Class Instances


    instance foldableArray :: Foldable Prim.Array


    instance foldableEither :: Foldable (Either a)


    instance foldableMaybe :: Foldable Maybe


    instance foldableTuple :: Foldable (Tuple a)


### Values


    all :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Boolean


    and :: forall f. (Foldable f) => f Boolean -> Boolean


    any :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Boolean


    elem :: forall a f. (Eq a, Foldable f) => a -> f a -> Boolean


    find :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Maybe a


    fold :: forall f m. (Foldable f, Monoid m) => f m -> m


    foldlArray :: forall a b. (b -> a -> b) -> b -> [a] -> b


    foldrArray :: forall a b. (a -> b -> b) -> b -> [a] -> b


    for_ :: forall a b f m. (Applicative m, Foldable f) => f a -> (a -> m b) -> m Unit


    intercalate :: forall f m. (Foldable f, Monoid m) => m -> f m -> m


    lookup :: forall a b f. (Eq a, Foldable f) => a -> f (Tuple a b) -> Maybe b


    mconcat :: forall f m. (Foldable f, Monoid m) => f m -> m


    notElem :: forall a f. (Eq a, Foldable f) => a -> f a -> Boolean


    or :: forall f. (Foldable f) => f Boolean -> Boolean


    product :: forall f. (Foldable f) => f Number -> Number


    sequence_ :: forall a f m. (Applicative m, Foldable f) => f (m a) -> m Unit


    sum :: forall f. (Foldable f) => f Number -> Number


    traverse_ :: forall a b f m. (Applicative m, Foldable f) => (a -> m b) -> f a -> m Unit


## Module Data.Traversable

### Type Classes


    class (Functor t, Foldable t) <= Traversable t where

      traverse :: forall a b m. (Applicative m) => (a -> m b) -> t a -> m (t b)

      sequence :: forall a m. (Applicative m) => t (m a) -> m (t a)


### Type Class Instances


    instance applicativeStateL :: Applicative (StateL s)


    instance applicativeStateR :: Applicative (StateR s)


    instance applyStateL :: Apply (StateL s)


    instance applyStateR :: Apply (StateR s)


    instance functorStateL :: Functor (StateL s)


    instance functorStateR :: Functor (StateR s)


    instance traversableArray :: Traversable Prim.Array


    instance traversableEither :: Traversable (Either a)


    instance traversableMaybe :: Traversable Maybe


    instance traversableTuple :: Traversable (Tuple a)


### Values


    for :: forall a b m t. (Applicative m, Traversable t) => t a -> (a -> m b) -> m (t b)


    mapAccumL :: forall a b s f. (Traversable f) => (s -> a -> Tuple s b) -> s -> f a -> Tuple s (f b)


    mapAccumR :: forall a b s f. (Traversable f) => (s -> a -> Tuple s b) -> s -> f a -> Tuple s (f b)


    scanl :: forall a b f. (Traversable f) => (b -> a -> b) -> b -> f a -> f b


    scanr :: forall a b f. (Traversable f) => (a -> b -> b) -> b -> f a -> f b


    zipWithA :: forall m a b c. (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]


## Module Data.Coyoneda

### Types


    newtype Coyoneda f a where
      Coyoneda :: Exists (CoyonedaF f a) -> Coyoneda f a


    newtype CoyonedaF f a i where
      CoyonedaF :: { fi :: f i, k :: i -> a } -> CoyonedaF f a i


    type Natural f g = forall a. f a -> g a


### Type Class Instances


    instance applicativeCoyoneda :: (Applicative f) => Applicative (Coyoneda f)


    instance applyCoyoneda :: (Apply f) => Apply (Coyoneda f)


    instance bindCoyoneda :: (Bind f) => Bind (Coyoneda f)


    instance comonadCoyoneda :: (Comonad w) => Comonad (Coyoneda w)


    instance extendCoyoneda :: (Extend w) => Extend (Coyoneda w)


    instance functorCoyoneda :: Functor (Coyoneda f)


    instance monadCoyoneda :: (Monad f) => Monad (Coyoneda f)


    instance monadTransCoyoneda :: MonadTrans Coyoneda


### Values


    coyoneda :: forall f a b. (a -> b) -> f a -> Coyoneda f b


    liftCoyoneda :: forall f a. f a -> Coyoneda f a


    liftCoyonedaT :: forall f g. Natural f g -> Natural (Coyoneda f) (Coyoneda g)


    liftCoyonedaTF :: forall f g. (Functor g) => Natural f g -> Natural (Coyoneda f) g


    lowerCoyoneda :: forall f a. (Functor f) => Coyoneda f a -> f a


## Module Data.Yoneda

### Types


    newtype Yoneda f a where
      Yoneda :: (forall b. (a -> b) -> f b) -> Yoneda f a


### Type Class Instances


    instance applicativeYoneda :: (Applicative f) => Applicative (Yoneda f)


    instance applyYoneda :: (Apply f) => Apply (Yoneda f)


    instance bindCoyoneda :: (Bind f) => Bind (Yoneda f)


    instance comonadYoneda :: (Comonad w) => Comonad (Yoneda w)


    instance extendYoneda :: (Extend w) => Extend (Yoneda w)


    instance functorYoneda :: Functor (Yoneda f)


    instance monadTransYoneda :: MonadTrans Yoneda


    instance monadYoneda :: (Monad f) => Monad (Yoneda f)


### Values


    liftYoneda :: forall f a. (Functor f) => f a -> Yoneda f a


    lowerYoneda :: forall f a. Yoneda f a -> f a


    runYoneda :: forall f a b. Yoneda f a -> (a -> b) -> f b


## Module Data.Identity

### Types


    newtype Identity a where
      Identity :: a -> Identity a


### Type Class Instances


    instance applicativeIdentity :: Applicative Identity


    instance applyIdentity :: Apply Identity


    instance bindIdentity :: Bind Identity


    instance comonadIdentity :: Comonad Identity


    instance eqIdentity :: (Eq a) => Eq (Identity a)


    instance extendIdentity :: Extend Identity


    instance foldableIdentity :: Foldable Identity


    instance functorIdentity :: Functor Identity


    instance monadIdentity :: Monad Identity


    instance ordIdentity :: (Ord a) => Ord (Identity a)


    instance showConst :: (Show a) => Show (Identity a)


    instance traversableIdentity :: Traversable Identity


### Values


    runIdentity :: forall a. Identity a -> a


## Module Data.Inject

### Type Classes


    class Inject f g where

      inj :: forall a. f a -> g a

      prj :: forall a. g a -> Maybe (f a)


### Type Class Instances


    instance injectLeft :: Inject f (Coproduct f g)


    instance injectReflexive :: Inject f f


    instance injectRight :: (Inject f g) => Inject f (Coproduct h g)


## Module Data.Lazy

### Types


    data Lazy :: * -> *


### Type Class Instances


    instance applicativeLazy :: Applicative Lazy


    instance applyLazy :: Apply Lazy


    instance bindLazy :: Bind Lazy


    instance comonadLazy :: Comonad Lazy


    instance eqLazy :: (Eq a) => Eq (Lazy a)


    instance extendLazy :: Extend Lazy


    instance functorLazy :: Functor Lazy


    instance lazy1Lazy :: CL.Lazy1 Lazy


    instance monadLazy :: Monad Lazy


    instance ordLazy :: (Ord a) => Ord (Lazy a)


    instance showLazy :: (Show a) => Show (Lazy a)


### Values


    defer :: forall a. (Unit -> a) -> Lazy a


    force :: forall a. Lazy a -> a


## Module Test.Data.List

### Type Class Instances


    instance arbitraryList :: (Arbitrary a) => Arbitrary (List a)


## Module Data.List

### Types


    data List a where
      Nil :: List a
      Cons :: a -> List a -> List a


### Type Class Instances


    instance altList :: Alt List


    instance alternativeList :: Alternative List


    instance applicativeList :: Applicative List


    instance applyList :: Apply List


    instance bindList :: Bind List


    instance eqList :: (Eq a) => Eq (List a)


    instance foldableList :: Foldable List


    instance functorList :: Functor List


    instance monadList :: Monad List


    instance monadPlusList :: MonadPlus List


    instance monoidList :: Monoid (List a)


    instance ordList :: (Ord a) => Ord (List a)


    instance plusList :: Plus List


    instance semigroupList :: Semigroup (List a)


    instance showList :: (Show a) => Show (List a)


    instance traversableList :: Traversable List


    instance unfoldableList :: Unfoldable List


### Values


    (!) :: forall a. List a -> Number -> Maybe a


    (\\) :: forall a. (Eq a) => List a -> List a -> List a


    alterAt :: forall a. Number -> (a -> Maybe a) -> List a -> Maybe (List a)


    catMaybes :: forall a. List (Maybe a) -> List a


    delete :: forall a. (Eq a) => a -> List a -> List a


    deleteAt :: forall a. Number -> List a -> Maybe (List a)


    deleteBy :: forall a. (a -> a -> Boolean) -> a -> List a -> List a


    drop :: forall a. Number -> List a -> List a


    filter :: forall a. (a -> Boolean) -> List a -> List a


    fromArray :: forall a. [a] -> List a


    group :: forall a. (Eq a) => List a -> List (List a)


    groupBy :: forall a. (a -> a -> Boolean) -> List a -> List (List a)


    head :: forall a. List a -> Maybe a


    init :: forall a. List a -> Maybe (List a)


    insert :: forall a. (Ord a) => a -> List a -> List a


    insertAt :: forall a. Number -> a -> List a -> Maybe (List a)


    insertBy :: forall a. (a -> a -> Ordering) -> a -> List a -> List a


    intersect :: forall a. (Eq a) => List a -> List a -> List a


    intersectBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a


    last :: forall a. List a -> Maybe a


    length :: forall a. List a -> Number


    mapMaybe :: forall a b. (a -> Maybe b) -> List a -> List b


    nub :: forall a. (Eq a) => List a -> List a


    nubBy :: forall a. (a -> a -> Boolean) -> List a -> List a


    null :: forall a. List a -> Boolean


    reverse :: forall a. List a -> List a


    span :: forall a. (a -> Boolean) -> List a -> Tuple (List a) (List a)


    tail :: forall a. List a -> Maybe (List a)


    take :: forall a. Number -> List a -> List a


    toArray :: forall a. List a -> [a]


    uncons :: forall a. List a -> Maybe (Tuple a (List a))


    union :: forall a. (Eq a) => List a -> List a -> List a


    unionBy :: forall a. (a -> a -> Boolean) -> List a -> List a -> List a


    zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c


## Module Data.Maybe

### Types


    data Maybe a where
      Nothing :: Maybe a
      Just :: a -> Maybe a


### Type Class Instances


    instance altMaybe :: Alt Maybe


    instance alternativeMaybe :: Alternative Maybe


    instance applicativeMaybe :: Applicative Maybe


    instance applyMaybe :: Apply Maybe


    instance bindMaybe :: Bind Maybe


    instance eqMaybe :: (Eq a) => Eq (Maybe a)


    instance extendMaybe :: Extend Maybe


    instance functorMaybe :: Functor Maybe


    instance monadMaybe :: Monad Maybe


    instance monadPlusMaybe :: MonadPlus Maybe


    instance ordMaybe :: (Ord a) => Ord (Maybe a)


    instance plusMaybe :: Plus Maybe


    instance semigroupMaybe :: (Semigroup a) => Semigroup (Maybe a)


    instance showMaybe :: (Show a) => Show (Maybe a)


### Values


    fromMaybe :: forall a. a -> Maybe a -> a


    isJust :: forall a. Maybe a -> Boolean


    isNothing :: forall a. Maybe a -> Boolean


    maybe :: forall a b. b -> (a -> b) -> Maybe a -> b


## Module Data.Monoid

### Type Classes


    class (Semigroup m) <= Monoid m where

      mempty :: m


### Type Class Instances


    instance monoidArr :: (Monoid b) => Monoid (a -> b)


    instance monoidArray :: Monoid [a]


    instance monoidMaybe :: (Semigroup a) => Monoid (Maybe a)


    instance monoidString :: Monoid String


    instance monoidUnit :: Monoid Unit


## Module Data.String

### Values


    charAt :: Number -> String -> Maybe Char


    charCodeAt :: Number -> String -> Maybe Number


    count :: (Char -> Boolean) -> String -> Number


    drop :: Number -> String -> String


    dropWhile :: (Char -> Boolean) -> String -> String


    fromChar :: Char -> String


    fromCharArray :: [Char] -> String


    indexOf :: String -> String -> Number


    indexOf' :: String -> Number -> String -> Number


    joinWith :: String -> [String] -> String


    lastIndexOf :: String -> String -> Number


    lastIndexOf' :: String -> Number -> String -> Number


    length :: String -> Number


    localeCompare :: String -> String -> Number


    null :: String -> Boolean


    replace :: String -> String -> String -> String


    singleton :: Char -> String


    split :: String -> String -> [String]


    take :: Number -> String -> String


    takeWhile :: (Char -> Boolean) -> String -> String


    toCharArray :: String -> [Char]


    toLower :: String -> String


    toUpper :: String -> String


    trim :: String -> String


    uncons :: String -> Maybe { tail :: String, head :: Char }


## Module Data.Tuple

### Types


    data Tuple a b where
      Tuple :: a -> b -> Tuple a b


### Type Class Instances


    instance applicativeTuple :: (Monoid a) => Applicative (Tuple a)


    instance applyTuple :: (Semigroup a) => Apply (Tuple a)


    instance bindTuple :: (Semigroup a) => Bind (Tuple a)


    instance comonadTuple :: Comonad (Tuple a)


    instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b)


    instance extendTuple :: Extend (Tuple a)


    instance functorTuple :: Functor (Tuple a)


    instance lazyLazy1Tuple :: (Lazy1 l1, Lazy1 l2) => Lazy (Tuple (l1 a) (l2 b))


    instance lazyLazy2Tuple :: (Lazy2 l1, Lazy2 l2) => Lazy (Tuple (l1 a b) (l2 c d))


    instance lazyTuple :: (Lazy a, Lazy b) => Lazy (Tuple a b)


    instance monadTuple :: (Monoid a) => Monad (Tuple a)


    instance monoidTuple :: (Monoid a, Monoid b) => Monoid (Tuple a b)


    instance ordTuple :: (Ord a, Ord b) => Ord (Tuple a b)


    instance semigroupTuple :: (Semigroup a, Semigroup b) => Semigroup (Tuple a b)


    instance semigroupoidTuple :: Semigroupoid Tuple


    instance showTuple :: (Show a, Show b) => Show (Tuple a b)


### Values


    curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c


    fst :: forall a b. Tuple a b -> a


    snd :: forall a b. Tuple a b -> b


    swap :: forall a b. Tuple a b -> Tuple b a


    uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c


    unzip :: forall a b. [Tuple a b] -> Tuple [a] [b]


    zip :: forall a b. [a] -> [b] -> [Tuple a b]


## Module Data.Tuple.Nested

### Values


    (/\) :: forall a b. a -> b -> Tuple a b


    con10 :: forall a b c d e f g h i j z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> z) -> Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h (Tuple i j)))))))) -> z


    con2 :: forall a b z. (a -> b -> z) -> Tuple a b -> z


    con3 :: forall a b c z. (a -> b -> c -> z) -> Tuple a (Tuple b c) -> z


    con4 :: forall a b c d z. (a -> b -> c -> d -> z) -> Tuple a (Tuple b (Tuple c d)) -> z


    con5 :: forall a b c d e z. (a -> b -> c -> d -> e -> z) -> Tuple a (Tuple b (Tuple c (Tuple d e))) -> z


    con6 :: forall a b c d e f z. (a -> b -> c -> d -> e -> f -> z) -> Tuple a (Tuple b (Tuple c (Tuple d (Tuple e f)))) -> z


    con7 :: forall a b c d e f g z. (a -> b -> c -> d -> e -> f -> g -> z) -> Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f g))))) -> z


    con8 :: forall a b c d e f g h z. (a -> b -> c -> d -> e -> f -> g -> h -> z) -> Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g h)))))) -> z


    con9 :: forall a b c d e f g h i z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> z) -> Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h i))))))) -> z


## Module Data.Unfoldable

### Type Classes


    class Unfoldable t where

      unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> t a


### Type Class Instances


    instance unfoldableArray :: Unfoldable Prim.Array


## Module Data.Array.ST

### Types


    data STArray :: * -> * -> *


### Values


    emptySTArray :: forall a h r. Eff (st :: ST h | r) (STArray h a)


    peekSTArray :: forall a h r. STArray h a -> Number -> Eff (st :: ST h | r) (Maybe a)


    pokeSTArray :: forall a h r. STArray h a -> Number -> a -> Eff (st :: ST h | r) Boolean


    pushSTArray :: forall a h r. STArray h a -> a -> Eff (st :: ST h | r) Unit


    runSTArray :: forall a r. (forall h. Eff (st :: ST h | r) (STArray h a)) -> Eff r [a]


## Module Data.Array.Unsafe

### Values


    head :: forall a. [a] -> a


    init :: forall a. [a] -> [a]


    last :: forall a. [a] -> a


    tail :: forall a. [a] -> [a]


## Module Data.Functor.Coproduct

### Types


    newtype Coproduct f g a where
      Coproduct :: Either (f a) (g a) -> Coproduct f g a


### Type Class Instances


    instance foldableCoproduct :: (Foldable f, Foldable g) => Foldable (Coproduct f g)


    instance functorCoproduct :: (Functor f, Functor g) => Functor (Coproduct f g)


    instance traversableCoproduct :: (Traversable f, Traversable g) => Traversable (Coproduct f g)


### Values


    coproduct :: forall f g a b. (f a -> b) -> (g a -> b) -> Coproduct f g a -> b


    left :: forall f g a. f a -> Coproduct f g a


    right :: forall f g a. g a -> Coproduct f g a


    runCoproduct :: forall f g a. Coproduct f g a -> Either (f a) (g a)


## Module Data.Either.Nested

### Values


    choice10 :: forall a b c d e f g h i j z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> (i -> z) -> (j -> z) -> Either a (Either b (Either c (Either d (Either e (Either f (Either g (Either h (Either i j)))))))) -> z


    choice2 :: forall a b z. (a -> z) -> (b -> z) -> Either a b -> z


    choice3 :: forall a b c z. (a -> z) -> (b -> z) -> (c -> z) -> Either a (Either b c) -> z


    choice4 :: forall a b c d z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> Either a (Either b (Either c d)) -> z


    choice5 :: forall a b c d e z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> Either a (Either b (Either c (Either d e))) -> z


    choice6 :: forall a b c d e f z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> Either a (Either b (Either c (Either d (Either e f)))) -> z


    choice7 :: forall a b c d e f g z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> Either a (Either b (Either c (Either d (Either e (Either f g))))) -> z


    choice8 :: forall a b c d e f g h z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> Either a (Either b (Either c (Either d (Either e (Either f (Either g h)))))) -> z


    choice9 :: forall a b c d e f g h i z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> (i -> z) -> Either a (Either b (Either c (Either d (Either e (Either f (Either g (Either h i))))))) -> z


## Module Control.Comonad.Cofree

### Types


    data Cofree f a


### Type Class Instances


    instance applicativeCofree :: (Applicative f) => Applicative (Cofree f)


    instance applyCofree :: (Apply f) => Apply (Cofree f)


    instance bindCofree :: (MonadPlus f) => Bind (Cofree f)


    instance comonadCofree :: (Functor f) => Comonad (Cofree f)


    instance extendCofree :: (Functor f) => Extend (Cofree f)


    instance foldableCofree :: (Foldable f) => Foldable (Cofree f)


    instance functorCofree :: (Functor f) => Functor (Cofree f)


    instance monadCofree :: (MonadPlus f) => Monad (Cofree f)


    instance traversableCofree :: (Traversable f) => Traversable (Cofree f)


### Values


    head :: forall f a. Cofree f a -> a


    mkCofree :: forall f a. a -> f (Cofree f a) -> Cofree f a


    tail :: forall f a. Cofree f a -> f (Cofree f a)


## Module Control.Monad.Free

### Types


    data Free f a where
      Pure :: a -> Free f a
      Free :: f (Free f a) -> Free f a
      Gosub :: (forall s. (forall r. (Unit -> Free f r) -> (r -> Free f a) -> s) -> s) -> Free f a


    type FreeC f = Free (Coyoneda f)


### Type Classes


    class MonadFree f m where

      wrap :: forall a. f (m a) -> m a


### Type Class Instances


    instance applicativeFree :: (Functor f) => Applicative (Free f)


    instance applyFree :: (Functor f) => Apply (Free f)


    instance bindFree :: (Functor f) => Bind (Free f)


    instance functorFree :: (Functor f) => Functor (Free f)


    instance monadFree :: (Functor f) => Monad (Free f)


    instance monadFreeFree :: (Functor f) => MonadFree f (Free f)


    instance monadTransFree :: MonadTrans Free


### Values


    go :: forall f a. (Functor f) => (f (Free f a) -> Free f a) -> Free f a -> a


    goEff :: forall e f a. (Functor f) => (f (Free f a) -> Eff e (Free f a)) -> Free f a -> Eff e a


    goEffC :: forall e f a. Natural f (Eff e) -> FreeC f a -> Eff e a

     Note: can blow the stack!

    goM :: forall f m a. (Functor f, Monad m) => (f (Free f a) -> m (Free f a)) -> Free f a -> m a

     Note: can blow the stack!

    goMC :: forall f m a. (Monad m) => Natural f m -> FreeC f a -> m a


    injC :: forall f g a. (Inject f g) => FreeC f a -> FreeC g a

     Note: can blow the stack!

    iterM :: forall f m a. (Functor f, Monad m) => (forall a. f (m a) -> m a) -> Free f a -> m a


    liftF :: forall f m a. (Functor f, Monad m, MonadFree f m) => f a -> m a


    liftFC :: forall f a. f a -> FreeC f a


    mapF :: forall f g a. (Functor f, Functor g) => Natural f g -> Free f a -> Free g a


    pureF :: forall f a. (Applicative f) => a -> Free f a


    pureFC :: forall f a. (Applicative f) => a -> FreeC f a


## Module Control.Monad.Trampoline

### Types


    type Trampoline = Free Lazy


### Values


    delay :: forall a. (Unit -> a) -> Trampoline a


    delay' :: forall a. Lazy a -> Trampoline a


    done :: forall a. a -> Trampoline a


    runTrampoline :: forall a. Trampoline a -> a


    suspend :: forall a. Trampoline a -> Trampoline a


## Module Data.Lazy.List

### Types


    data List a where
      Nil :: List a
      Cons :: a -> Lazy (List a) -> List a


### Type Class Instances


    instance applicativeList :: Applicative List


    instance applyList :: Apply List


    instance bindList :: Bind List


    instance eqList :: (Eq a) => Eq (List a)


    instance functorList :: Functor List


    instance monadList :: Monad List


    instance monoidList :: Monoid (List a)


    instance semigroupList :: Semigroup (List a)


    instance showList :: (Show a) => Show (List a)


### Values


    drop :: forall a. Number -> List a -> List a


    fromArray :: forall a. [a] -> List a


    repeat :: forall a. a -> List a


    take :: forall a. Number -> List a -> List a


    toArray :: forall a. List a -> [a]


## Module Test.Control.Monad.ListT

### Type Class Instances


    instance arbitraryListT :: (Monad f, Arbitrary a) => Arbitrary (ListT f a)


## Module Control.Monad.ListT

### Types


    data ListT f a


### Type Class Instances


    instance altListT :: (Applicative f) => Alt (ListT f)


    instance alternativeListT :: (Monad f) => Alternative (ListT f)


    instance applicativeListT :: (Monad f) => Applicative (ListT f)


    instance applyListT :: (Monad f) => Apply (ListT f)


    instance bindListT :: (Monad f) => Bind (ListT f)


    instance functorListT :: (Functor f) => Functor (ListT f)


    instance monadListT :: (Monad f) => Monad (ListT f)


    instance monadPlusListT :: (Monad f) => MonadPlus (ListT f)


    instance monadTransListT :: MonadTrans ListT


    instance monoidListT :: (Applicative f) => Monoid (ListT f a)


    instance plusListT :: (Monad f) => Plus (ListT f)


    instance semigroupListT :: (Applicative f) => Semigroup (ListT f a)


    instance unfoldableListT :: (Monad f) => Unfoldable (ListT f)


### Values


    catMaybes :: forall f a. (Functor f) => ListT f (Maybe a) -> ListT f a


    cons' :: forall f a. (Applicative f) => Lazy a -> Lazy (ListT f a) -> ListT f a


    drop :: forall f a. (Applicative f) => Number -> ListT f a -> ListT f a


    dropWhile :: forall f a. (Applicative f) => (a -> Boolean) -> ListT f a -> ListT f a


    filter :: forall f a. (Functor f) => (a -> Boolean) -> ListT f a -> ListT f a


    foldl :: forall f a b. (Monad f) => (b -> a -> b) -> b -> ListT f a -> f b


    foldl' :: forall f a b. (Monad f) => (b -> a -> f b) -> b -> ListT f a -> f b


    fromArray :: forall f a. (Monad f) => [a] -> ListT f a


    fromEffect :: forall f a. (Applicative f) => f a -> ListT f a


    head :: forall f a. (Monad f) => ListT f a -> f (Maybe a)


    iterate :: forall f a. (Monad f) => (a -> a) -> a -> ListT f a


    mapMaybe :: forall f a b. (Functor f) => (a -> Maybe b) -> ListT f a -> ListT f b


    nil :: forall f a. (Applicative f) => ListT f a


    prepend :: forall f a. (Applicative f) => a -> ListT f a -> ListT f a


    prepend' :: forall f a. (Applicative f) => a -> Lazy (ListT f a) -> ListT f a


    repeat :: forall f a. (Monad f) => a -> ListT f a


    scanl :: forall f a b. (Monad f) => (b -> a -> b) -> b -> ListT f a -> ListT f b


    singleton :: forall f a. (Applicative f) => a -> ListT f a


    tail :: forall f a. (Monad f) => ListT f a -> f (Maybe (ListT f a))


    take :: forall f a. (Applicative f) => Number -> ListT f a -> ListT f a


    takeWhile :: forall f a. (Applicative f) => (a -> Boolean) -> ListT f a -> ListT f a


    toArray :: forall f a. (Monad f) => ListT f a -> f [a]


    uncons :: forall f a. (Monad f) => ListT f a -> f (Maybe (Tuple a (ListT f a)))


    unfold :: forall f a z. (Monad f) => (z -> f (Maybe (Tuple z a))) -> z -> ListT f a


    wrapEffect :: forall f a. (Monad f) => f (ListT f a) -> ListT f a


    wrapLazy :: forall f a. (Monad f) => Lazy (ListT f a) -> ListT f a


    zipWith :: forall f a b c. (Monad f) => (a -> b -> c) -> ListT f a -> ListT f b -> ListT f c


    zipWith' :: forall f a b c. (Monad f) => (a -> b -> f c) -> ListT f a -> ListT f b -> ListT f c


## Module Data.List.Unsafe

### Values


    head :: forall a. List a -> a


    init :: forall a. List a -> List a


    last :: forall a. List a -> a


    tail :: forall a. List a -> List a


## Module Data.Maybe.Unsafe

### Values


    fromJust :: forall a. Maybe a -> a


## Module Data.Monoid.All

### Types


    newtype All where
      All :: Boolean -> All


### Type Class Instances


    instance eqAll :: Eq All


    instance monoidAll :: Monoid All


    instance semigroupAll :: Semigroup All


    instance showAll :: Show All


### Values


    runAll :: All -> Boolean


## Module Data.Monoid.Any

### Types


    newtype Any where
      Any :: Boolean -> Any


### Type Class Instances


    instance eqAny :: Eq Any


    instance monoidAny :: Monoid Any


    instance semigroupAny :: Semigroup Any


    instance showAny :: Show Any


### Values


    runAny :: Any -> Boolean


## Module Data.Monoid.Dual

### Types


    newtype Dual a where
      Dual :: a -> Dual a


### Type Class Instances


    instance eqDual :: (Eq a) => Eq (Dual a)


    instance monoidDual :: (Monoid a) => Monoid (Dual a)


    instance ordDual :: (Ord a) => Ord (Dual a)


    instance semigroupDual :: (Semigroup a) => Semigroup (Dual a)


    instance showDual :: (Show a) => Show (Dual a)


### Values


    runDual :: forall a. Dual a -> a


## Module Data.Monoid.Endo

### Types


    newtype Endo a where
      Endo :: (a -> a) -> Endo a


### Type Class Instances


    instance monoidEndo :: Monoid (Endo a)


    instance semigroupEndo :: Semigroup (Endo a)


### Values


    runEndo :: forall a. Endo a -> a -> a


## Module Data.Monoid.First

### Types


    newtype First a where
      First :: Maybe a -> First a


### Type Class Instances


    instance eqFirst :: (Eq a) => Eq (First a)


    instance monoidFirst :: Monoid (First a)


    instance ordFirst :: (Ord a) => Ord (First a)


    instance semigroupFirst :: Semigroup (First a)


    instance showFirst :: (Show a) => Show (First a)


### Values


    runFirst :: forall a. First a -> Maybe a


## Module Data.Monoid.Last

### Types


    newtype Last a where
      Last :: Maybe a -> Last a


### Type Class Instances


    instance eqLast :: (Eq a) => Eq (Last a)


    instance monoidLast :: Monoid (Last a)


    instance ordLast :: (Ord a) => Ord (Last a)


    instance semigroupLast :: Semigroup (Last a)


    instance showLast :: (Show a) => Show (Last a)


### Values


    runLast :: forall a. Last a -> Maybe a


## Module Data.Monoid.Product

### Types


    newtype Product where
      Product :: Number -> Product


### Type Class Instances


    instance eqProduct :: Eq Product


    instance monoidProduct :: Monoid Product


    instance ordProduct :: Ord Product


    instance semigroupProduct :: Semigroup Product


    instance showProduct :: Show Product


### Values


    runProduct :: Product -> Number


## Module Data.Monoid.Sum

### Types


    newtype Sum where
      Sum :: Number -> Sum


### Type Class Instances


    instance eqSum :: Eq Sum


    instance monoidSum :: Monoid Sum


    instance ordSum :: Ord Sum


    instance semigroupSum :: Semigroup Sum


    instance showSum :: Show Sum


### Values


    runSum :: Sum -> Number


## Module Data.Char

### Types


    newtype Char


### Type Class Instances


    instance eqChar :: Eq Char


    instance ordChar :: Ord Char


    instance showChar :: Show Char


### Values


    charString :: Char -> String


    fromCharCode :: Number -> Char


    toCharCode :: Char -> Number


## Module Data.String.Regex

### Types


    data Regex :: *


    type RegexFlags = { unicode :: Boolean, sticky :: Boolean, multiline :: Boolean, ignoreCase :: Boolean, global :: Boolean }


### Type Class Instances


    instance showRegex :: Show Regex


### Values


    flags :: Regex -> RegexFlags


    match :: Regex -> String -> Maybe [String]


    parseFlags :: String -> RegexFlags


    regex :: String -> RegexFlags -> Regex


    renderFlags :: RegexFlags -> String


    replace :: Regex -> String -> String -> String


    replace' :: Regex -> (String -> [String] -> String) -> String -> String


    search :: Regex -> String -> Number


    source :: Regex -> String


    split :: Regex -> String -> [String]


    test :: Regex -> String -> Boolean


## Module Data.String.Unsafe

### Values


    charAt :: Number -> String -> Char


    charCodeAt :: Number -> String -> Number


## Module Control.Comonad.Env

### Types


    type Env e = EnvT e Identity


### Values


    env :: forall e a. e -> a -> Env e a


    mapEnv :: forall e a b. (a -> b) -> Env e a -> Env e b


    runEnv :: forall e a. Env e a -> Tuple e a


    withEnv :: forall e1 e2 a. (e1 -> e2) -> Env e1 a -> Env e2 a


## Module Control.Comonad.Store

### Types


    type Store s a = StoreT s Identity a


### Values


    runStore :: forall s a. Store s a -> Tuple (s -> a) s


    store :: forall s a. (s -> a) -> s -> Store s a


## Module Control.Comonad.Traced

### Types


    type Traced m = TracedT m Identity


### Values


    runTraced :: forall m a. Traced m a -> m -> a


    traced :: forall m a. (m -> a) -> Traced m a


## Module Control.Comonad.Trans

### Type Classes


    class ComonadTrans f where

      lower :: forall w a. (Comonad w) => f w a -> w a


## Module Control.Monad.Error

### Type Classes


    class Error a where

      noMsg :: a

      strMsg :: String -> a


### Type Class Instances


    instance errorString :: Error String


## Module Control.Monad.RWS

### Types


    type RWS r w s = RWST r w s Identity


### Values

     | Reader operations

    ask :: forall r w s m. (Applicative m, Monoid w) => RWST r w s m r


    censor :: forall r w s m a. (Monad m) => (w -> w) -> RWST r w s m a -> RWST r w s m a


    evalRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple a w


    execRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple s w


    get :: forall r w s m. (Applicative m, Monoid w) => RWST r w s m s


    gets :: forall r w s m a. (Applicative m, Monoid w) => (s -> a) -> RWST r w s m a


    listen :: forall r w s m a. (Monad m) => RWST r w s m a -> RWST r w s m (Tuple a w)


    listens :: forall r w s m a b. (Monad m) => (w -> b) -> RWST r w s m a -> RWST r w s m (Tuple a b)


    local :: forall r w s m a. (r -> r) -> RWST r w s m a -> RWST r w s m a


    mapRWS :: forall r w1 w2 s a1 a2. (See s a1 w1 -> See s a2 w2) -> RWS r w1 s a1 -> RWS r w2 s a2


    modify :: forall r w s m. (Applicative m, Monoid w) => (s -> s) -> RWST r w s m Unit


    pass :: forall r w s m a. (Monad m) => RWST r w s m (Tuple a (w -> w)) -> RWST r w s m a


    put :: forall r w s m. (Applicative m, Monoid w) => s -> RWST r w s m Unit


    reader :: forall r w s m a. (Applicative m, Monoid w) => (r -> a) -> RWST r w s m a


    runRWS :: forall r w s a. RWS r w s a -> r -> s -> See s a w


    rws :: forall r w s a. (r -> s -> See s a w) -> RWS r w s a

     | State operations

    state :: forall r w s m a. (Applicative m, Monoid w) => (s -> Tuple a s) -> RWST r w s m a


    tell :: forall r w s m. (Applicative m) => w -> RWST r w s m Unit


    withRWS :: forall r1 r2 w s a. (r2 -> s -> Tuple r1 s) -> RWS r1 w s a -> RWS r2 w s a

     | Writer operations

    writer :: forall r w s m a. (Applicative m) => Tuple a w -> RWST r w s m a


## Module Control.Monad.Reader

### Types


    type Reader r = ReaderT r Identity


### Values


    mapReader :: forall r a b. (a -> b) -> Reader r a -> Reader r b


    runReader :: forall r a. Reader r a -> r -> a


    withReader :: forall r1 r2 a b. (r2 -> r1) -> Reader r1 a -> Reader r2 a


## Module Control.Monad.State

### Types


    type State s = StateT s Identity


### Values


    evalState :: forall s a. State s a -> s -> a


    execState :: forall s a. State s a -> s -> s


    mapState :: forall s a b. (Tuple a s -> Tuple b s) -> State s a -> State s b


    runState :: forall s a. State s a -> s -> Tuple a s


    withState :: forall s a. (s -> s) -> State s a -> State s a


## Module Control.Monad.Trans

### Type Classes


    class MonadTrans t where

      lift :: forall m a. (Monad m) => m a -> t m a


## Module Control.Monad.Writer

### Types


    type Writer w = WriterT w Identity


### Values


    execWriter :: forall w a. Writer w a -> w


    mapWriter :: forall w1 w2 a b. (Tuple a w1 -> Tuple b w2) -> Writer w1 a -> Writer w2 b


    runWriter :: forall w a. Writer w a -> Tuple a w


## Module Data.List.Lazy

### Types


    newtype LazyList a where
      LazyList :: List a -> LazyList a


    type List = L.ListT Lazy


### Type Class Instances


    instance foldableLazyList :: Foldable LazyList


### Values


    unLazyList :: forall a. LazyList a -> List a


## Module Control.Comonad.Env.Class

### Type Classes


    class (Comonad w) <= ComonadEnv e w where

      ask :: forall a. w a -> e

      local :: forall a. (e -> e) -> w a -> w a


### Type Class Instances


    instance comonadEnvEnvT :: (Comonad w) => ComonadEnv e (EnvT e w)


    instance comonadEnvTuple :: ComonadEnv e (Tuple e)


### Values


    asks :: forall e1 e2 w a. (ComonadEnv e1 w) => (e1 -> e2) -> w e1 -> e2


## Module Control.Comonad.Env.Trans

### Types


    newtype EnvT e w a where
      EnvT :: Tuple e (w a) -> EnvT e w a


### Type Class Instances


    instance comonadEnvT :: (Comonad w) => Comonad (EnvT e w)


    instance comonadTransEnvT :: ComonadTrans (EnvT e)


    instance extendEnvT :: (Extend w) => Extend (EnvT e w)


    instance functorEnvT :: (Functor w) => Functor (EnvT e w)


### Values


    mapEnvT :: forall e w1 w2 a b. (w1 a -> w2 b) -> EnvT e w1 a -> EnvT e w2 b


    runEnvT :: forall e w a. EnvT e w a -> Tuple e (w a)


    withEnvT :: forall e1 e2 w a. (e1 -> e2) -> EnvT e1 w a -> EnvT e2 w a


## Module Control.Comonad.Store.Class

### Type Classes


    class (Comonad w) <= ComonadStore s w where

      pos :: forall a. w a -> s

      peek :: forall a. s -> w a -> a


### Type Class Instances


    instance comonadStoreStoreT :: (Comonad w) => ComonadStore s (StoreT s w)


### Values


    experiment :: forall f a w s. (ComonadStore s w, Functor f) => (s -> f s) -> w a -> f a


    peeks :: forall s a w. (ComonadStore s w) => (s -> s) -> w a -> a


    seek :: forall s a w. (ComonadStore s w, Extend w) => s -> w a -> w a


    seeks :: forall s a w. (ComonadStore s w, Extend w) => (s -> s) -> w a -> w a


## Module Control.Comonad.Store.Trans

### Types


    newtype StoreT s w a where
      StoreT :: Tuple (w (s -> a)) s -> StoreT s w a


### Type Class Instances


    instance comonadStoreT :: (Comonad w) => Comonad (StoreT s w)


    instance comonadTransStoreT :: ComonadTrans (StoreT s)


    instance extendStoreT :: (Extend w) => Extend (StoreT s w)


    instance functorStoreT :: (Functor w) => Functor (StoreT s w)


### Values


    runStoreT :: forall s w a. StoreT s w a -> Tuple (w (s -> a)) s


## Module Control.Comonad.Traced.Class

### Type Classes


    class (Comonad w) <= ComonadTraced t w where

      track :: forall a. t -> w a -> a


### Type Class Instances


    instance comonadTracedTracedT :: (Comonad w, Monoid t) => ComonadTraced t (TracedT t w)


### Values


    censor :: forall w a t b. (Functor w) => (t -> t) -> TracedT t w a -> TracedT t w a


    listen :: forall w a t. (Functor w) => TracedT t w a -> TracedT t w (Tuple a t)


    listens :: forall w a t b. (Functor w) => (t -> b) -> TracedT t w a -> TracedT t w (Tuple a b)


    tracks :: forall w a t. (Comonad w, ComonadTraced t w) => (a -> t) -> w a -> a


## Module Control.Comonad.Traced.Trans

### Types


    newtype TracedT t w a where
      TracedT :: w (t -> a) -> TracedT t w a


### Type Class Instances


    instance comonadTracedT :: (Comonad w, Monoid t) => Comonad (TracedT t w)


    instance comonadTransTracedT :: (Monoid t) => ComonadTrans (TracedT t)


    instance extendTracedT :: (Extend w, Semigroup t) => Extend (TracedT t w)


    instance functorTracedT :: (Functor w) => Functor (TracedT t w)


### Values


    runTracedT :: forall w a t. TracedT t w a -> w (t -> a)


## Module Control.Monad.Cont.Class

### Type Classes


    class MonadCont m where

      callCC :: forall a b. ((a -> m b) -> m a) -> m a


### Type Class Instances


    instance monadContContT :: (Monad m) => MonadCont (Cont.ContT r m)


    instance monadContErrorT :: (Error e, MonadCont m) => MonadCont (ErrorT e m)


    instance monadContMaybeT :: (MonadCont m) => MonadCont (MaybeT m)


    instance monadContReaderT :: (MonadCont m) => MonadCont (ReaderT r m)


    instance monadContStateT :: (MonadCont m) => MonadCont (StateT s m)


    instance monadWriterT :: (Monoid w, MonadCont m) => MonadCont (WriterT w m)


## Module Control.Monad.Cont.Trans

### Types


    newtype ContT r m a where
      ContT :: ((a -> m r) -> m r) -> ContT r m a


### Type Class Instances


    instance applicativeContT :: (Functor m, Monad m) => Applicative (ContT r m)


    instance applyContT :: (Functor m, Monad m) => Apply (ContT r m)


    instance bindContT :: (Monad m) => Bind (ContT r m)


    instance functorContT :: (Monad m) => Functor (ContT r m)


    instance monadContT :: (Monad m) => Monad (ContT r m)


    instance monadTransContT :: MonadTrans (ContT r)


### Values


    callCC :: forall r m a b. ((a -> ContT r m b) -> ContT r m a) -> ContT r m a


    mapContT :: forall r m a. (m r -> m r) -> ContT r m a -> ContT r m a


    runContT :: forall r m a. ContT r m a -> (a -> m r) -> m r


    withContT :: forall r m a b. ((b -> m r) -> a -> m r) -> ContT r m a -> ContT r m b


## Module Control.Monad.Error.Class

### Type Classes


    class MonadError e m where

      throwError :: forall a. e -> m a

      catchError :: forall a. m a -> (e -> m a) -> m a


### Type Class Instances


    instance monadErrorError :: (Error e) => MonadError e (Either e)


    instance monadErrorErrorT :: (Monad m, Error e) => MonadError e (ErrorT e m)


    instance monadErrorMaybeT :: (Monad m, MonadError e m) => MonadError e (MaybeT m)


    instance monadErrorReaderT :: (Monad m, MonadError e m) => MonadError e (ReaderT r m)


    instance monadErrorStateT :: (Monad m, MonadError e m) => MonadError e (StateT s m)


    instance monadErrorWriterT :: (Monad m, Monoid w, MonadError e m) => MonadError e (WriterT w m)


## Module Control.Monad.Error.Trans

### Types


    newtype ErrorT e m a where
      ErrorT :: m (Either e a) -> ErrorT e m a


### Type Class Instances


    instance altErrorT :: (Monad m, Error e) => Alt (ErrorT e m)


    instance alternativeErrorT :: (Monad m, Error e) => Alternative (ErrorT e m)


    instance applicativeErrorT :: (Applicative m) => Applicative (ErrorT e m)


    instance applyErrorT :: (Apply m) => Apply (ErrorT e m)


    instance bindErrorT :: (Monad m, Error e) => Bind (ErrorT e m)


    instance functorErrorT :: (Functor m) => Functor (ErrorT e m)


    instance monadErrorT :: (Monad m, Error e) => Monad (ErrorT e m)


    instance monadPlusErrorT :: (Monad m, Error e) => MonadPlus (ErrorT e m)


    instance monadTransErrorT :: (Error e) => MonadTrans (ErrorT e)


    instance plusErrorT :: (Monad m, Error e) => Plus (ErrorT e m)


### Values


    liftCallCCError :: forall e m a b. (((Either e a -> m (Either e b)) -> m (Either e a)) -> m (Either e a)) -> ((a -> ErrorT e m b) -> ErrorT e m a) -> ErrorT e m a


    liftListenError :: forall e m a w. (Monad m) => (m (Either e a) -> m (Tuple (Either e a) w)) -> ErrorT e m a -> ErrorT e m (Tuple a w)


    liftPassError :: forall e m a w. (Monad m) => (m (Tuple (Either e a) (w -> w)) -> m (Either e a)) -> ErrorT e m (Tuple a (w -> w)) -> ErrorT e m a


    mapErrorT :: forall e1 e2 m1 m2 a b. (m1 (Either e1 a) -> m2 (Either e2 b)) -> ErrorT e1 m1 a -> ErrorT e2 m2 b


    runErrorT :: forall e m a. ErrorT e m a -> m (Either e a)


## Module Control.Monad.Maybe.Trans

### Types


    newtype MaybeT m a where
      MaybeT :: m (Maybe a) -> MaybeT m a


### Type Class Instances


    instance applicativeMaybeT :: (Monad m) => Applicative (MaybeT m)


    instance applyMaybeT :: (Monad m) => Apply (MaybeT m)


    instance bindMaybeT :: (Monad m) => Bind (MaybeT m)


    instance functorMaybeT :: (Monad m) => Functor (MaybeT m)


    instance monadMaybeT :: (Monad m) => Monad (MaybeT m)


    instance monadTransMaybeT :: MonadTrans MaybeT


### Values


    liftCallCCMaybe :: forall m a b. (((Maybe a -> m (Maybe b)) -> m (Maybe a)) -> m (Maybe a)) -> ((a -> MaybeT m b) -> MaybeT m a) -> MaybeT m a


    liftCatchMaybe :: forall m e a. (m (Maybe a) -> (e -> m (Maybe a)) -> m (Maybe a)) -> MaybeT m a -> (e -> MaybeT m a) -> MaybeT m a


    liftListenMaybe :: forall m a w. (Monad m) => (m (Maybe a) -> m (Tuple (Maybe a) w)) -> MaybeT m a -> MaybeT m (Tuple a w)


    liftPassMaybe :: forall m a w. (Monad m) => (m (Tuple (Maybe a) (w -> w)) -> m (Maybe a)) -> MaybeT m (Tuple a (w -> w)) -> MaybeT m a


    mapMaybeT :: forall m1 m2 a b. (m1 (Maybe a) -> m2 (Maybe b)) -> MaybeT m1 a -> MaybeT m2 b


    runMaybeT :: forall m a. MaybeT m a -> m (Maybe a)


## Module Control.Monad.RWS.Class

### Type Classes


    class (Monad m, Monoid w, MonadReader r m, MonadWriter w m, MonadState s m) <= MonadRWS r w s m where


### Type Class Instances


    instance monadRWSErrorT :: (Monad m, Monoid w, MonadRWS r w s m, MonadReader r m, MonadWriter w m, MonadState s m, Error e) => MonadRWS r w s (ErrorT e m)


    instance monadRWSMaybeT :: (Monad m, Monoid w, MonadRWS r w s m, MonadReader r m, MonadWriter w m, MonadState s m) => MonadRWS r w s (MaybeT m)


    instance monadRWSRWST :: (Monad m, Monoid w) => MonadRWS r w s (RWST r w s m)


## Module Control.Monad.RWS.Trans

### Types


    newtype RWST r w s m a where
      RWST :: (r -> s -> m (See s a w)) -> RWST r w s m a


    type See s a w = { log :: w, result :: a, state :: s }


### Type Class Instances


    instance applicativeRWST :: (Applicative m, Monoid w) => Applicative (RWST r w s m)


    instance applyRWST :: (Apply m, Semigroup w) => Apply (RWST r w s m)


    instance bindRWST :: (Bind m, Semigroup w) => Bind (RWST r w s m)


    instance functorRWST :: (Functor m) => Functor (RWST r w s m)


    instance monadRWST :: (Monad m, Monoid w) => Monad (RWST r w s m)


    instance monadTransRWST :: (Monoid w) => MonadTrans (RWST r w s)


### Values


    evalRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple a w)


    execRWST :: forall r w s m a. (Monad m) => RWST r w s m a -> r -> s -> m (Tuple s w)


    mapRWST :: forall r w1 w2 s m1 m2 a1 a2. (m1 (See s a1 w1) -> m2 (See s a2 w2)) -> RWST r w1 s m1 a1 -> RWST r w2 s m2 a2


    mkSee :: forall s a w. (Monoid w) => s -> a -> w -> See s a w


    runRWST :: forall r w s m a. RWST r w s m a -> r -> s -> m (See s a w)


    withRWST :: forall r1 r2 w s m a. (r2 -> s -> Tuple r1 s) -> RWST r1 w s m a -> RWST r2 w s m a


## Module Control.Monad.Reader.Class

### Type Classes


    class MonadReader r m where

      ask :: m r

      local :: forall a. (r -> r) -> m a -> m a


### Type Class Instances


    instance monadReaderErrorT :: (Monad m, Error e, MonadReader r m) => MonadReader r (ErrorT e m)


    instance monadReaderFun :: MonadReader r (Prim.Function r)


    instance monadReaderMaybeT :: (Monad m, MonadReader r m) => MonadReader r (MaybeT m)


    instance monadReaderRWST :: (Monad m, Monoid w) => MonadReader r (RWST r w s m)


    instance monadReaderReaderT :: (Monad m) => MonadReader r (ReaderT r m)


    instance monadReaderStateT :: (Monad m, MonadReader r m) => MonadReader r (StateT s m)


    instance monadReaderWriterT :: (Monad m, Monoid w, MonadReader r m) => MonadReader r (WriterT w m)


### Values


    reader :: forall r m a. (Monad m, MonadReader r m) => (r -> a) -> m a


## Module Control.Monad.Reader.Trans

### Types


    newtype ReaderT r m a where
      ReaderT :: (r -> m a) -> ReaderT r m a


### Type Class Instances


    instance altReaderT :: (Alt m) => Alt (ReaderT r m)


    instance alternativeReaderT :: (Alternative m) => Alternative (ReaderT r m)


    instance applicativeReaderT :: (Applicative m) => Applicative (ReaderT r m)


    instance applyReaderT :: (Applicative m) => Apply (ReaderT r m)


    instance bindReaderT :: (Monad m) => Bind (ReaderT r m)


    instance functorReaderT :: (Functor m) => Functor (ReaderT r m)


    instance monadPlusReaderT :: (MonadPlus m) => MonadPlus (ReaderT r m)


    instance monadReaderT :: (Monad m) => Monad (ReaderT r m)


    instance monadTransReaderT :: MonadTrans (ReaderT r)


    instance plusReaderT :: (Plus m) => Plus (ReaderT r m)


### Values


    liftCallCCReader :: forall r m a b. (((a -> m b) -> m a) -> m a) -> ((a -> ReaderT r m b) -> ReaderT r m a) -> ReaderT r m a


    liftCatchReader :: forall r m e a. (m a -> (e -> m a) -> m a) -> ReaderT r m a -> (e -> ReaderT r m a) -> ReaderT r m a


    liftReaderT :: forall r m a. m a -> ReaderT r m a


    mapReaderT :: forall r m1 m2 a b. (m1 a -> m2 b) -> ReaderT r m1 a -> ReaderT r m2 b


    runReaderT :: forall r m a. ReaderT r m a -> r -> m a


    withReaderT :: forall r1 r2 m a b. (r2 -> r1) -> ReaderT r1 m a -> ReaderT r2 m a


## Module Control.Monad.State.Class

### Type Classes


    class MonadState s m where

      state :: forall a. (s -> Tuple a s) -> m a


### Type Class Instances


    instance monadStateErrorT :: (Monad m, Error e, MonadState s m) => MonadState s (ErrorT e m)


    instance monadStateMaybeT :: (Monad m, MonadState s m) => MonadState s (MaybeT m)


    instance monadStateRWST :: (Monad m, Monoid w) => MonadState s (RWST r w s m)


    instance monadStateReaderT :: (Monad m, MonadState s m) => MonadState s (ReaderT r m)


    instance monadStateStateT :: (Monad m) => MonadState s (StateT s m)


    instance monadStateStateT1 :: (Monad m, MonadState s m) => MonadState s (StateT s1 m)


    instance monadStateWriterT :: (Monad m, Monoid w, MonadState s m) => MonadState s (WriterT w m)


### Values


    get :: forall m s. (Monad m, MonadState s m) => m s


    gets :: forall s m a. (Monad m, MonadState s m) => (s -> a) -> m a


    modify :: forall s m. (Monad m, MonadState s m) => (s -> s) -> m Unit


    put :: forall m s. (Monad m, MonadState s m) => s -> m Unit


## Module Control.Monad.State.Trans

### Types


    newtype StateT s m a where
      StateT :: (s -> m (Tuple a s)) -> StateT s m a


### Type Class Instances


    instance altStateT :: (Monad m, Alt m) => Alt (StateT s m)


    instance alternativeStateT :: (Monad m, Alternative m) => Alternative (StateT s m)


    instance applicativeStateT :: (Monad m) => Applicative (StateT s m)


    instance applyStateT :: (Monad m) => Apply (StateT s m)


    instance bindStateT :: (Monad m) => Bind (StateT s m)


    instance functorStateT :: (Monad m) => Functor (StateT s m)


    instance lazy1StateT :: Lazy1 (StateT s m)


    instance monadPlusStateT :: (MonadPlus m) => MonadPlus (StateT s m)


    instance monadStateT :: (Monad m) => Monad (StateT s m)


    instance monadTransStateT :: MonadTrans (StateT s)


    instance plusStateT :: (Monad m, Plus m) => Plus (StateT s m)


### Values


    evalStateT :: forall s m a. (Apply m) => StateT s m a -> s -> m a


    execStateT :: forall s m a. (Apply m) => StateT s m a -> s -> m s


    liftCallCCState :: forall s m a b. (((Tuple a s -> m (Tuple b s)) -> m (Tuple a s)) -> m (Tuple a s)) -> ((a -> StateT s m b) -> StateT s m a) -> StateT s m a


    liftCallCCState' :: forall s m a b. (((Tuple a s -> m (Tuple b s)) -> m (Tuple a s)) -> m (Tuple a s)) -> ((a -> StateT s m b) -> StateT s m a) -> StateT s m a


    liftCatchState :: forall s m e a. (m (Tuple a s) -> (e -> m (Tuple a s)) -> m (Tuple a s)) -> StateT s m a -> (e -> StateT s m a) -> StateT s m a


    liftListenState :: forall s m a w. (Monad m) => (m (Tuple a s) -> m (Tuple (Tuple a s) w)) -> StateT s m a -> StateT s m (Tuple a w)


    liftPassState :: forall s m a b w. (Monad m) => (m (Tuple (Tuple a s) b) -> m (Tuple a s)) -> StateT s m (Tuple a b) -> StateT s m a


    mapStateT :: forall s m1 m2 a b. (m1 (Tuple a s) -> m2 (Tuple b s)) -> StateT s m1 a -> StateT s m2 b


    runStateT :: forall s m a. StateT s m a -> s -> m (Tuple a s)


    withStateT :: forall s m a. (s -> s) -> StateT s m a -> StateT s m a


## Module Control.Monad.Writer.Class

### Type Classes


    class MonadWriter w m where

      writer :: forall a. Tuple a w -> m a

      listen :: forall a. m a -> m (Tuple a w)

      pass :: forall a. m (Tuple a (w -> w)) -> m a


### Type Class Instances


    instance monadWriterErrorT :: (Monad m, Error e, MonadWriter w m) => MonadWriter w (ErrorT e m)


    instance monadWriterMaybeT :: (Monad m, MonadWriter w m) => MonadWriter w (MaybeT m)


    instance monadWriterRWST :: (Monad m, Monoid w) => MonadWriter w (RWST r w s m)


    instance monadWriterReaderT :: (Monad m, MonadWriter w m) => MonadWriter w (ReaderT r m)


    instance monadWriterStateT :: (Monad m, MonadWriter w m) => MonadWriter w (StateT s m)


    instance monadWriterWriterT :: (Monoid w, Monad m) => MonadWriter w (WriterT w m)


### Values


    censor :: forall w m a. (Monoid w, Monad m, MonadWriter w m) => (w -> w) -> m a -> m a


    listens :: forall w m a b. (Monoid w, Monad m, MonadWriter w m) => (w -> b) -> m a -> m (Tuple a b)


    tell :: forall w m a. (Monoid w, Monad m, MonadWriter w m) => w -> m Unit


## Module Control.Monad.Writer.Trans

### Types


    newtype WriterT w m a where
      WriterT :: m (Tuple a w) -> WriterT w m a


### Type Class Instances


    instance altWriterT :: (Monoid w, Alt m) => Alt (WriterT w m)


    instance alternativeWriterT :: (Monoid w, Alternative m) => Alternative (WriterT w m)


    instance applicativeWriterT :: (Monoid w, Applicative m) => Applicative (WriterT w m)


    instance applyWriterT :: (Monoid w, Apply m) => Apply (WriterT w m)


    instance bindWriterT :: (Monoid w, Monad m) => Bind (WriterT w m)


    instance functorWriterT :: (Functor m) => Functor (WriterT w m)


    instance monadPlusWriterT :: (Monoid w, MonadPlus m) => MonadPlus (WriterT w m)


    instance monadTransWriterT :: (Monoid w) => MonadTrans (WriterT w)


    instance monadWriterT :: (Monoid w, Monad m) => Monad (WriterT w m)


    instance plusWriterT :: (Monoid w, Plus m) => Plus (WriterT w m)


### Values


    execWriterT :: forall w m a. (Apply m) => WriterT w m a -> m w


    liftCallCCWriter :: forall w m a b. (Monoid w) => (((Tuple a w -> m (Tuple b w)) -> m (Tuple a w)) -> m (Tuple a w)) -> ((a -> WriterT w m b) -> WriterT w m a) -> WriterT w m a


    liftCatchWriter :: forall w m e a. (m (Tuple a w) -> (e -> m (Tuple a w)) -> m (Tuple a w)) -> WriterT w m a -> (e -> WriterT w m a) -> WriterT w m a


    mapWriterT :: forall w1 w2 m1 m2 a b. (m1 (Tuple a w1) -> m2 (Tuple b w2)) -> WriterT w1 m1 a -> WriterT w2 m2 b


    runWriterT :: forall w m a. WriterT w m a -> m (Tuple a w)



