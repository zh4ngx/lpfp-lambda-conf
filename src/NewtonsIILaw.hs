module NewtonsIILaw where

import Graphics.Gnuplot.Simple
import DescribingMotion


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

-- | Newton's second law with constant force
carGraph :: IO ()
carGraph
    = plotFunc [Title "Car on an air track"
               ,XLabel "Time (s)"
               ,YLabel "Velocity of Car (m/s)"
               ,PNG "car.png"
               ] [0..4 :: Time] (velocityCF 0.1 0.6 [0.04, -0.08])

-- | Newton's second law with force that changes with time
-- for numerical integration
velocityFt :: R -- dt for integral
              ->  Mass
              -> Velocity -- velocity
              -> [Time -> Force]  -- forces
              -> VelocityFunction

velocityFt dt m v0 fs =
    let fNet t = sum [f t | f <- fs]
        a t = fNet t / m
    in antiderivative dt v0 a

positionFt :: R -- dt for integral
              -> Mass
              -> Position -- position
              -> Velocity
              -> [Time -> Force]  -- forces
              -> PositionFunction
positionFt dt m x0 v0 fs =
    antiderivative dt x0 (velocityFt dt m v0 fs)

pedalCoast :: Time -> Force
pedalCoast t
    = let tCycle = 20
          nComplete :: Int
          nComplete = truncate (t / tCycle)
          remainder = t - fromIntegral nComplete * tCycle
      in if remainder < 10
         then 10
         else 0

childPedaling :: IO ()
childPedaling
    = plotFunc [Title "Child pedaling then coasting"
               ,XLabel "Time (s)"
               ,YLabel "Position of Bike (m)"
            --   ,PNG "ChildPosition.png" -- update when creating a progrma
            --    ,Key Nothing -- update when craeting a program
               ] [0..40 :: R] (positionFt 0.1 20 0 0 [pedalCoast])



fAir :: R  -- drag coefficient
     -> R  -- air density
     -> R  -- cross-sectional area of object
     -> Velocity
     -> Force
fAir drag rho area v = - (drag * rho * area * abs v * v / 2)

newtonSecondV :: Mass
                -> [Velocity -> Force]
                -> Velocity
                -> R
newtonSecondV m fs v0
    = sum [f v0 | f <- fs] / m

-- update Velocityt usnig the euler method

updateVelocity :: R -- dt
                  -> Mass
                  -> [Velocity -> Force]
                  -> Velocity
                  -> Velocity
updateVelocity dt m fs v0
    = v0 + dt * newtonSecondV m fs v0

velocityFv :: R -- dt
              -> Mass
              -> Velocity
              -> [Velocity -> Force]
              -> VelocityFunction
velocityFv dt m v0 fs t =
    let numSteps = abs $ round (t / dt)
    in iterate (updateVelocity dt m fs) v0 !! numSteps

bikeVelocity :: VelocityFunction
bikeVelocity = velocityFv 1 70 0 [const 100, fAir 2 1.225 0.6]

bikeGraph :: IO ()
bikeGraph = plotFunc [Title "Bike velocity"
                     ,XLabel "Time (s)"
                     ,YLabel "Velocity of Bike (m/s)"
                    --  ,PNG "BikeVelocity1.png"
                    --  ,Key Nothing
                     ] [0,0.5..60] bikeVelocity