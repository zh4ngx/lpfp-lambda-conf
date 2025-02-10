module NewtonsIILaw where

import Graphics.Gnuplot.Simple

type R = Double
type Mass     = R
type Time     = R
type Position = R
type Velocity = R
type Acceleration = R
type Force    = R
type VelocityFunction = Time -> Velocity
type PositionFunction = Time -> Position
type AccelerationFunction = Time -> Acceleration

velocityCF :: Mass 
              -> Velocity -- velocity
              -> [Force]  -- forces
              -> VelocityFunction
velocityCF m v0 fs = 
    let
     a = sum fs / m
    in \t -> v0 + a * t

positionCF :: Mass 
            -> Position -- position
            -> Velocity 
            -> [Force]  -- forces
            -> PositionFunction
positionCF m x0 v0 fs =
    let
      fNet = sum fs
      a0 = fNet / m
      x t = x0 + v0 * t + 0.5 * a0 * t ** 2
    in x

-- | Newton's second law
carGraph :: IO ()
carGraph
    = plotFunc [Title "Car on an air track"
               ,XLabel "Time (s)"
               ,YLabel "Velocity of Car (m/s)"
               ] [0..4 :: Time] (velocityCF 0.1 0.6 [0.04, -0.08])