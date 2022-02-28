module Ch18MonadExercises where

import           Control.Monad

-- Create bind in terms of fmap and join
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f
