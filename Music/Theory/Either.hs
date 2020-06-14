-- | Either
module Music.Theory.Either where

import Data.Maybe {- base -}

-- | Maybe 'Left' of 'Either'.
from_left :: Either a b -> Maybe a
from_left e =
    case e of
      Left x -> Just x
      _ -> Nothing

-- | 'fromJust' of 'from_left'
from_left_err :: Either t e -> t
from_left_err = fromMaybe (error "from_left_err") . from_left

-- | Maybe 'Right' of 'Either'.
from_right :: Either x t -> Maybe t
from_right e =
    case e of
      Left _ -> Nothing
      Right r -> Just r

-- | 'fromJust' of 'from_right'
from_right_err :: Either e t -> t
from_right_err = fromMaybe (error "from_right_err") . from_right

-- | Flip from right to left, ie. 'either' 'Right' 'Left'
either_swap :: Either a b -> Either b a
either_swap = either Right Left
