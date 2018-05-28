
module Purity.NormalFlightPhysics where

import Linear
import Control.Lens
import Control.Monad
import Data.Foldable
import Purity.Data

newtype Constraint q a = Constraint
  {_normalComponent :: q a
  } deriving (Eq,Ord)

-- Physics object can be in normal contact with a constraining surface.
-- This is represented by pairs


