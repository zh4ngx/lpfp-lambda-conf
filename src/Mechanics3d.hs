module Mechanics3d where
import DescribingMotion(R)

import Vectors
    ( Vec, PosVec, (^+^), (^-^), (*^), (^*), (^/), (<.>), (><)
    , vec, sumV, magnitude, zeroV, xComp, yComp, zComp, iHat, jHat, kHat)
data ParticleState = ParticleState { mass     :: R
                                   , charge   :: R
                                   , time     :: R
                                   , posVec   :: Vec
                                   , velocity :: Vec }
                     deriving Show


defaultParticleState :: ParticleState
defaultParticleState = ParticleState { mass     = 1
                                     , charge   = 0
                                     , time     = 0
                                     , posVec   = zeroV
                                     , velocity = zeroV }

rockState :: ParticleState
rockState
    = defaultParticleState { mass     = 2                        -- kg
                           , velocity = 3 *^ iHat ^+^ 4 *^ kHat  -- m/s
                           }

type OneBodyForce = ParticleState -> Vec


