module Animation.Easing where

import Extra.Prelude
import Math (pow)

-- An easing is a function from [0,1] where f(0) = 1 and f(1) = 1
newtype Easing = Easing (Number -> Number)

ease :: Easing -> Number -> Number
ease (Easing f) = f

-- Given a number between 0 and 1 and two easing functions,
-- creates a piecewise easing function that compresses them and glues them together.
connectAt :: Number -> Easing -> Easing -> Easing
connectAt x (Easing l) (Easing r) = Easing \t ->
  if t <= x
    then l $ t / x
    else r $ (t-x) / (1.0-x)

-- Given two easing functions,
-- creates a piecewise easing function that compresses them and glues them together.
connect :: Easing -> Easing -> Easing
connect l r = connectAt 0.5 l r

mult :: Number -> Easing -> Easing
mult x (Easing f) = Easing \t -> x * f t

instance semiringEasing :: Semiring Easing where
  add (Easing f) (Easing g) = Easing \t -> f t + g t
  mul (Easing f) (Easing g) = Easing \t -> f t * g t
  zero = Easing \t -> 0.0
  one = Easing \t -> 1.0

instance ringEasing :: Ring Easing where
  sub (Easing f) (Easing g) = Easing \t -> f t - g t

--------------------------------------------------------------------------------
-- Cubic hermite polynomials
--     | f 0 | f 1 | f' 0 | f' 1
-- h00 |  1  |  0  |  0   |  0
-- h01 |  0  |  1  |  0   |  0
-- h10 |  0  |  0  |  1   |  0
-- h11 |  0  |  0  |  0   |  1
--------------------------------------------------------------------------------
-- f(0) == 1
h00 :: Easing
h00 = Easing \t -> 2.0 * pow t 3.0 - 3.0 * pow t 2.0 + 1.0

-- f(1) == 1
h01 :: Easing
h01 = Easing \t -> -2.0 * pow t 3.0 + 3.0 * pow t 2.0

-- f'(0) == 1
h10 :: Easing
h10 = Easing \t -> pow t 3.0 - 2.0 * pow t 2.0 + t

-- f'(1) == 1
h11 :: Easing
h11 = Easing \t -> pow t 3.0 - pow t 2.0

