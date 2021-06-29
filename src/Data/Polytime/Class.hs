{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Typeclass for fetching times.
-}

module Data.Polytime.Class where

import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Trans.Reader (ReaderT, ask)

-- | Fetch the current time of a particular type.
class TimeSource time f where
  -- | What time is it now?
  now :: f time

-- | Extract the current time from a @time@ 'ReaderT'
--
-- This overlaps the generic lifting through Monad transformers.
instance {-# OVERLAPPING #-} Monad f => TimeSource time (ReaderT time f) where
  now = ask

-- | Lift 'TimeSource' through Monad transformers.
instance {-# OVERLAPPABLE #-} (TimeSource time m, MonadTrans t, Monad m) => TimeSource time (t m) where
    now = lift now
