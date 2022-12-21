{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Ledger.Constraints.ValidityInterval where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Interval (Extended (Finite, NegInf, PosInf), Interval (Interval), LowerBound (LowerBound),
                                  UpperBound (UpperBound))
import PlutusTx.Prelude (Bool (False, True), Enum (succ), Functor (fmap), Maybe (Just, Nothing))
import Prelude qualified as Haskell

-- | 'ValidityInterval' is a half open interval. Closed (inclusive) on the bottom, open
-- (exclusive) on the top. A 'Nothing' on the bottom is negative infinity, and a 'Nothing'
-- on the top is positive infinity.
data ValidityInterval a = ValidityInterval
  { invalidBefore    :: !(Maybe a) -- ^ Inclusive lower bound or negative infinity
  , invalidHereafter :: !(Maybe a) -- ^ Exclusive upper bound or positive infinity
  }
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (ToJSON, FromJSON)

instance Functor ValidityInterval where
  fmap f (ValidityInterval from to) = ValidityInterval (fmap f from) (fmap f to)

{-# INLINABLE fromLowerBound #-}
fromLowerBound :: Enum a => LowerBound a -> Maybe a
fromLowerBound (LowerBound (Finite v) closed) = if closed then Just v else Just (succ v)
fromLowerBound _                              = Nothing

{-# INLINABLE fromUpperBound #-}
fromUpperBound :: Enum a => UpperBound a -> Maybe a
fromUpperBound (UpperBound (Finite v) closed) = if closed then Just (succ v) else Just v
fromUpperBound _                              = Nothing

{-# INLINABLE fromPlutusInterval #-}
fromPlutusInterval :: Enum a => Interval a -> ValidityInterval a
fromPlutusInterval (Interval from' to') = ValidityInterval (fromLowerBound from') (fromUpperBound to')

{-# INLINABLE toLowerBound #-}
toLowerBound :: Maybe a -> LowerBound a
toLowerBound (Just v) = LowerBound (Finite v) True
toLowerBound _        = LowerBound NegInf True

{-# INLINABLE toUpperBound #-}
toUpperBound :: Maybe a -> UpperBound a
toUpperBound (Just v) = UpperBound (Finite v) False
toUpperBound _        = UpperBound PosInf True

{-# INLINABLE toPlutusInterval #-}
toPlutusInterval :: ValidityInterval a -> Interval a
toPlutusInterval (ValidityInterval from' to') = Interval (toLowerBound from') (toUpperBound to')
