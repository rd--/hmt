-- | Monad functions.
module Music.Theory.Monad where

repeatM_ :: (Monad m) => m a -> m ()
repeatM_ = sequence_ . repeat

iterateM_ :: (Monad m) => st -> (st -> m st) -> m ()
iterateM_ st f = do
  st' <- f st
  iterateM_ st' f
