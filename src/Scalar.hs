{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}

module Scalar where

import           Data.Ratio

instance {-# OVERLAPPABLE #-} (Show n, Scalar s n) => Show (s n) where
  show v = (show $ scalarVal v) ++ suffix v

instance (Eq n, Scalar s n) => Eq (s n) where
  v == v' = scalarVal v == scalarVal v'

instance (Ord n, Scalar s n) => Ord (s n) where
  v <= v' = scalarVal v <= scalarVal v'

class RealFrac n =>
      Scalar s n
  where
  scalar :: n -> s n
  suffix :: s n -> String
  scalarVal :: s n -> n
  (.*) :: n -> s n -> s n
  n .* v = scalar (n * scalarVal v)
  {-# MINIMAL scalar, suffix, scalarVal #-}
