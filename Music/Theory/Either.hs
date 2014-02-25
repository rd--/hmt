-- | Either
module Music.Theory.Either where

-- | Maybe 'Left' of 'Either'.
fromLeft :: Either a b -> Maybe a
fromLeft e =
    case e of
      Left x -> Just x
      _ -> Nothing

-- | Maybe 'Right' of 'Either'.
fromRight :: Either a b -> Maybe b
fromRight e =
    case e of
      Right x -> Just x
      _ -> Nothing
