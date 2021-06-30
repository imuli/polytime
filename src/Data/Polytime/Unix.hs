{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Unix time.
-}

module Data.Polytime.Unix
  ( Unix(..)
  ) where

import           Data.Polytime.Class (TimeSource, now)
import           Data.Polytime.Span (Span(Span))
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           Linear.Affine (Affine, Diff, (.+^), (.-.), (.-^))
import           Linear.Vector ((^+^), (^-^))

-- | Unix clock time.
--
-- The time, offset from 1970-01-01 00:00:00 UTC. Note that this is an offset
-- within UTC, not an absolute number of seconds passed. Each day is defined to
-- have exactly 86400 seconds, and there are occasional leap seconds to keep
-- the end of day synchronized with the rotation of the earth.
newtype Unix n = Unix (Span n)
  deriving
     ( Bounded
     , Enum
     , Eq
     , Foldable
     , Fractional
     , Functor
     , Generic
     , Num
     , Ord
     , Read
     , Real
     , RealFrac
     , Storable
     , Traversable
     )

instance Affine Unix where
  type instance Diff Unix = Span
  Unix p .+^ x = Unix $ p ^+^ x
  Unix p .-. Unix q = p ^-^ q
  Unix p .-^ q = Unix $ p ^-^ q

instance Fractional n => TimeSource (Unix n) IO where
  now = Unix . Span . realToFrac <$> getPOSIXTime
