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

-- | 'fmap' of 'concat' of 'mapM'
concatMapM :: Monad m => (t -> m [u]) -> [t] -> m [u]
concatMapM f = fmap concat . mapM f

-- | If i then j else k.
m_if :: Monad m => (m Bool,m t,m t) -> m t
m_if (i,j,k) = do
  r <- i
  if r then j else k
