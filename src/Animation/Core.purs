module Animation.Core
 ( DiffTime (..)
 , plus
 , minus
 , Animated (..)
 , singleton
 , animate
 , static
 , Animating
 , resolve
 , isStatic
 , prune
 )  where

import Extra.Prelude

import Data.List.Lazy.NonEmpty (NonEmptyList)
import Data.List.Lazy.NonEmpty as NEL
import Data.Exists (Exists, runExists, mkExists)
import Data.DateTime.Instant (unInstant, instant)
import Data.Time.Duration (Milliseconds (..))

newtype DiffTime = DiffTime Number -- a difference between timestamps

derive instance newtypeDiffTime :: Newtype DiffTime _
derive newtype instance eqDiffTime :: Eq DiffTime
derive newtype instance ordDiffTime :: Ord DiffTime
derive newtype instance semiringDiffTime :: Semiring DiffTime

unwrapInstant :: Instant -> Number
unwrapInstant = unInstant >>> un Milliseconds

wrapInstant :: Number -> Instant
wrapInstant = Milliseconds >>> instant >>> unsafeFromJust

minus :: Instant -> Instant -> DiffTime
minus start end = DiffTime $ unwrapInstant start - unwrapInstant end

plus :: Instant -> DiffTime -> Instant
plus s (DiffTime diff) = wrapInstant (unwrapInstant s + diff)

newtype Animated a =
    Animated (NonEmptyList { duration :: DiffTime, value :: DiffTime -> a })

singleton :: forall a. DiffTime -> (DiffTime -> a) -> Animated a
singleton duration value = Animated (NEL.singleton {duration, value})

derive instance newtypeAnimated :: Newtype (Animated a) _
derive instance functorAnimated :: Functor Animated

animate :: forall a. Instant -> Animated a -> Animating a
animate startTime animation = Animating startTime animation

static :: forall a. a -> Animated a
static x = Animated (pure { duration : zero, value : const x})

data Animating a =
    Animating Instant (Animated a)
  | AnimatingAp (ExAp Animating a)

instance animatingSemiring :: Semiring a => Semiring (Animating a) where
  add x y = add <$> x <*> y
  zero = pure zero
  mul x y = mul <$> x <*> y
  one = pure one

newtype Ap f a x = Ap { fn :: f (x -> a), value :: f x }
newtype ExAp f a = ExAp (Exists (Ap f a))

instance functorExAp :: Functor f => Functor (ExAp f) where
  map f (ExAp e) = ExAp $ runExists
   (\(Ap {fn, value}) ->
     let fn' = map (\g -> f <<< g) fn
      in mkExists (Ap {fn: fn', value})
   ) e

derive instance functorAnimating :: Functor Animating

instance applicativeAnimating :: Applicative Animating where
  pure x = Animating bottom (static x)

instance applyAnimating :: Apply Animating where
  apply fn value = AnimatingAp $ ExAp $
    mkExists $ Ap { fn, value }

getStatic :: forall a. Instant -> Animating a -> Maybe a
-- Ap can only be static if both components are. This function assumed that
-- the argument is already pruned, and prune can't return an `Ap` where both
-- components are static (Exercise).
getStatic _ (AnimatingAp _) = Nothing
-- This doesn't walk down the `next` values because they're not relevant
-- on pruned data. If this is pruned, and if there is a next thing in the
-- list, then this one is not expired.
getStatic now (Animating startTime (Animated as)) =
  let {duration, value} = NEL.head as
   in if (now `minus` startTime) < duration
    then Nothing
    else Just (value duration)

prune :: forall a. Instant -> Animating a -> Animating a
prune now a@(Animating start (Animated animated)) =
  let { head: {duration}, tail } = NEL.uncons animated
   in case NEL.fromList tail of
        Nothing -> a -- tail is empty
        Just rest -> -- tail is nonempty
          if (now `minus` start) > duration
            then prune now $ Animating (start `plus` duration) (Animated rest)
            else a
prune now (AnimatingAp (ExAp e)) = runExists (
  \(Ap {fn, value}) ->
    let fn' = prune now fn
        value' = prune now value
     in case getStatic now fn', getStatic now value' of
         Just f, Just v -> pure (f v)
         Just f, _ ->  f <$> value'
         _, Just v -> (\f -> f v) <$> fn'
         Nothing, Nothing -> fn' <*> value'
  ) e

applyClamped :: forall a. DiffTime -> (DiffTime -> a) -> DiffTime -> a
applyClamped bound f t =
  if t < zero
    then f zero
    else if t > bound
           then f bound
           else f t

-- Uses a time to convert an animation into a value, assuming that it is
-- in canonical form
unsafeResolve :: forall a. Instant -> Animating a -> a
unsafeResolve t (AnimatingAp (ExAp e)) = e # runExists \(Ap {fn, value}) ->
  unsafeResolve t fn (unsafeResolve t value)
unsafeResolve t (Animating startTime (Animated as)) =
  let {duration, value} = NEL.head as
   in applyClamped duration value (t `minus` startTime)

-- Uses a time to convert an animation into a value.
resolve :: forall a. Instant -> Animating a -> a
resolve t = unsafeResolve t <<< prune t

-- determines whether the animation has reached the end of its animating time
isStatic :: forall a. Instant -> Animating a -> Boolean
isStatic t a = isJust (getStatic t $ prune t a)

