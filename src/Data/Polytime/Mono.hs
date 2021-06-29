{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Monotonic time.
-}

module Data.Polytime.Mono
  ( Mono(..)
  ) where

import           Data.Polytime.Class (TimeSource, now)
import           Data.Polytime.Span (Span(Span))
import           Foreign.Storable (Storable)
import           GHC.Clock (getMonotonicTimeNSec)
import           GHC.Generics (Generic)

-- | Monotonic clock time.
--
-- This has an arbitrary epoch, and usually based off of how long the system
-- has been running (for some sense of running). The only guarentee is that
-- this clock will not run backwards. In particular, if you sample
-- 'Data.Polytime.Unix' or 'Data.Polytime.Tai' time and 'Mono' time together,
-- and then do it again after some interval, you are not guarenteed to end up
-- with anything close to the same 'Span' between.
newtype Mono n = Mono (Span n)
  deriving
     ( Applicative
     , Bounded
     , Enum
     , Eq
     , Foldable
     , Fractional
     , Functor
     , Generic
     , Monad
     , Num
     , Ord
     , Real
     , RealFrac
     , Storable
     , Traversable
     )

instance Fractional n => TimeSource (Mono n) IO where
  now = Mono . Span . (/ 1000000000) . fromIntegral <$> getMonotonicTimeNSec
