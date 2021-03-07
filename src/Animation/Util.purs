module Animation.Util where

import Extra.Prelude

import Animation.Core as A
import Animation.Core (Animating, DiffTime (..))
import Animation.Easing as E

slide :: Instant -> DiffTime -> Vector Number -> Vector (Animating Number)
slide startTime duration v =  f <$> v
  where
  ease :: Number -> Number
  ease = E.ease $
           ( E.h01
           + E.mult 2.0 E.h10
           - E.mult 1.0 E.h11
           )
  f x = A.animate startTime $
    A.singleton
      duration
      \(DiffTime elapsed) -> (ease $ elapsed / un DiffTime duration) * x

bump :: Instant -> DiffTime -> Vector Number -> Vector (Animating Number)
bump startTime duration v = f <$> v
  where
  f x = A.animate startTime $
    A.singleton
      duration
     \(DiffTime elapsed) -> E.ease E.h10 (elapsed / un DiffTime duration) * x


