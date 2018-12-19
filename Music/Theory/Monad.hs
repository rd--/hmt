-- | Monad functions.
module Music.Theory.Monad where

-- | 'sequence_' of 'repeat'.
repeatM_ :: Monad m => m a -> m ()
repeatM_ = sequence_ . repeat

-- | Monadic variant of 'iterate'.
iterateM_ :: Monad m => (st -> m st) -> st -> m ()
iterateM_ f st = do
  st' <- f st
  iterateM_ f st'
