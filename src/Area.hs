{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}

module Area where

import           Data.Ratio
import           Distance
import           Scalar

data Acre n where
  Acre :: RealFrac n => n -> Acre n

data Square s n where
  SquareArea :: (RealFrac n, Distance s n) => n -> Square s n

squareAreaFromSides :: (RealFrac n, Distance s n) => s n -> s n -> Square s n
squareAreaFromSides v v' = SquareArea $ distVal v * distVal v'

squareAreaFromSide :: (RealFrac n, Distance s n) => s n -> Square s n
squareAreaFromSide v = squareAreaFromSides v v

squareAreaFromRealFrac :: (RealFrac n, Distance s n) => n -> Square s n
squareAreaFromRealFrac v = squareAreaFromSide (dist v)

instance RealFrac n => Scalar Acre n where
  scalar = Acre
  suffix _ = "ac"
  scalarVal (Acre v) = v

instance (RealFrac n, Distance s n) => Scalar (Square s) n where
  scalar = SquareArea
  suffix _ = distSuffix ++ "^2"
    where
      distSuffix = suffix $ (dist 0 :: s n)
  scalarVal (SquareArea v) = v

instance RealFrac n => Area Acre n where
  toMetric (Acre v) = SquareArea $ v * acreMultiplier
    where
      acreMultiplier =
        fromRational
          ((distVal $toMetre $ Foot (66 :: Rational)) *
           (distVal $toMetre $ Foot (660 :: Rational)))

instance (RealFrac n, Distance s n) => Area (Square s) n where
  toMetric (SquareArea v) = area $ v * sideLengthSquared
    where
      sideLengthSquared = (distVal $ (toMetre :: s n -> Metre n) $ dist 1) ^ 2

class (RealFrac n, Scalar a n) =>
      Area a n
  where
  toMetric :: a n -> Square Metre n
  toMetric v =
    area $
    areaVal v / (areaVal ((fromMetric (area 1 :: Square Metre n)) :: a n))
  fromMetric :: Square Metre n -> a n
  fromMetric (SquareArea v) = area $ v / (areaVal $ toMetric $ (area 1 :: a n))
  convertBetweenArea :: (Area a' n) => a' n -> a n
  convertBetweenArea = fromMetric . toMetric
  area :: n -> a n
  area = scalar
  areaVal :: a n -> n
  areaVal = scalarVal
  {-# MINIMAL fromMetric | toMetric #-}
