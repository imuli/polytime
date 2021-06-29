{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Time Spans.
-}

module Data.Polytime.Span
  ( Span(..)
  ) where

import           Control.Applicative (liftA2)
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)

-- | Time Spans, or differences between two times in seconds.
newtype Span n = Span n
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
     , Real
     , RealFrac
     , Storable
     , Traversable
     )

instance Applicative Span where
  pure = Span
  Span f <*> a = f <$> a
  liftA2 f (Span a) (Span b) = Span $ f a b

instance Monad Span where
  Span a >>= f = f a
