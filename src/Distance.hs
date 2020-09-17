{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}

module Distance where

import           Data.Ratio
import           Scalar

data Metre n where
  Metre :: RealFrac n => n -> Metre n

data Yard n where
  Yard :: RealFrac n => n -> Yard n

data Inch n where
  Inch :: RealFrac n => n -> Inch n

data Foot n where
  Foot :: RealFrac n => n -> Foot n

data Mile n where
  Mile :: RealFrac n => n -> Mile n

data AU n where
  AU :: RealFrac n => n -> AU n

instance RealFrac n => Scalar Metre n where
  scalar = Metre
  suffix _ = "m"
  scalarVal (Metre v) = v

instance RealFrac n => Scalar Yard n where
  scalar = Yard
  suffix _ = "yd"
  scalarVal (Yard v) = v

instance RealFrac n => Scalar Inch n where
  scalar = Inch
  suffix _ = "'"
  scalarVal (Inch v) = v

instance RealFrac n => Scalar Foot n where
  scalar = Foot
  suffix _ = "\""
  scalarVal (Foot v) = v

instance RealFrac n => Scalar Mile n where
  scalar = Mile
  suffix _ = "mi"
  scalarVal (Mile v) = v

instance RealFrac n => Scalar AU n where
  scalar = AU
  suffix _ = "AU"
  scalarVal (AU v) = v

instance forall n. RealFrac n => Distance Metre n where
  toMetre = id

instance RealFrac n => Distance Yard n where
  toMetre (Yard v) = Metre (0.9144 * v)

instance RealFrac n => Distance Inch n where
  toMetre (Inch v) = Metre (0.0254 * v)

instance RealFrac n => Distance Foot n where
  toMetre (Foot v) = Metre (0.3048 * v)

instance RealFrac n => Distance Mile n where
  toMetre (Mile v) = Metre (1609.344 * v)

instance RealFrac n => Distance AU n where
  toMetre (AU v) = Metre (149597870700 * v)

class (RealFrac n, Scalar s n) =>
      Distance s n
  where
  toMetre :: s n -> Metre n
  toMetre v = Metre $ distVal v / (distVal ((fromMetre (Metre 1)) :: s n))
  fromMetre :: Metre n -> s n
  fromMetre (Metre v) = dist $ v / (distVal $ toMetre $ (dist 1 :: s n))
  toMetreConst :: s n -> n
  toMetreConst _ = (distVal $ toMetre $ (dist 1 :: s n))
  convertBetweenDistance :: Distance s' n => s' n -> s n
  convertBetweenDistance = fromMetre . toMetre
  dist :: n -> s n
  dist = scalar
  distVal :: s n -> n
  distVal = scalarVal
  {-# MINIMAL toMetre | fromMetre #-}
