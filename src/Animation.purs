module Animation
  ( module Animation.Core
  , module Animation.Util
  ) where


import Animation.Core
 ( DiffTime (..)
 , plus
 , minus
 , Animated (..)
 , singleton
 , animate
 , static
 , Animating
 , resolve
 , prune
 )
import Animation.Util
  (slide, bump)
