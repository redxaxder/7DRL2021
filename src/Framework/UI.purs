module Framework.UI where

import Extra.Prelude
import Control.Monad.Free as Free
import Control.Monad.Free (Free)

data UIF gs a e ui input r =
    UIAction a (Either e gs -> r)
  | UIAwaitingInput ui (input -> r)

derive instance functorUIF :: Functor (UIF gs a e ui input)

--type parameters:
-- gs: gameState
-- a: game actions
-- e: errors denoting failed game actions
-- ui: ui state
-- i: input
-- r: placeholder for results
type UIM gs a e ui i r = Free (UIF gs a e ui i) r

input :: forall gs a e ui i. ui -> UIM gs a e ui i i
input ui = Free.liftF (UIAwaitingInput ui identity)

action :: forall gs a e ui i. a -> UIM gs a e ui i (Either e gs)
action a = Free.liftF (UIAction a identity)

getUIState :: forall gs a e ui i r. UIM gs a e ui i r -> Maybe ui
getUIState uim = case (Free.resume uim) of
  Right _ -> Nothing
  Left (UIAction _ _) -> Nothing
  Left (UIAwaitingInput s _) -> Just s
