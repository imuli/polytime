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
import           Linear.Vector (Additive, zero, (^+^), (^-^))

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

instance Additive Span where
  zero = Span 0
  (^+^) = (+)
  (^-^) = (-)

instance Show n => Show (Span n) where
  showsPrec prec (Span n) = showsPrec prec n

instance Applicative Span where
  pure = Span
  Span f <*> a = f <$> a
  liftA2 f (Span a) (Span b) = Span $ f a b

instance Monad Span where
  Span a >>= f = f a
