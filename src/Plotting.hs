module Plotting where

import Graphics.Gnuplot.Simple
import Graphics.Gloss
import DescribingMotion (Position, Mass, Velocity, Time, Force, R)

tRange :: [R]
tRange = [0,0.1..5]

yPos :: R -> R -> R -> R -> R
yPos y0 vy0 ay t = y0 + vy0*t + 0.5*ay*t^(2 :: Integer)

yVel :: R -> R -> R -> R
yVel vy0 ay t = vy0 + ay*t

yAcc :: R -> R
yAcc x = -9.8

solvingNewtonSecondLaw :: a
solvingNewtonSecondLaw = undefined



plot1 :: IO ()
plot1 =
  plotFunc [
    Title "Position(t)",
    XLabel "Time (seconds)",
    YLabel "Height of Proyectile",
    PNG "projectile.png"
    ] tRange (yPos 0 20 (-9.8))

plot2 :: IO ()
plot2 =
  plotFunc [
    Title "Velocity(t)",
    XLabel "Time (seconds)",
    YLabel "Velocity of Proyectile",
    PNG "vel-projectile.png"
    ] tRange (yVel 20 (-9.8))


plot3 :: IO ()
plot3 =
  plotFunc [
    Title "Acceleration(t)",
    XLabel "Time (seconds)",
    YLabel "Acceleration of Proyectile",
    PNG "accel-projectile.png"
    ] tRange yAcc


positionFt :: R                 -- dt for integral
           -> Mass
           -> Position          -- initial position
           -> Velocity          -- initial velocity
           -> [Time -> Force]   -- list of force functions
           -> Time -> Position  -- position function
positionFt dt m x0 v0 fs
    = antiDerivative dt x0 (velocityFt dt m v0 fs)

integral :: R -> (R -> R) -> R -> R -> R
integral dt f a b
    = sum [f t * dt | t <- [a+dt/2, a+3*dt/2 .. b - dt/2]]

antiDerivative :: R -> R -> (R -> R) -> (R -> R)
antiDerivative dt v0 a t = v0 + integral dt a 0 t

velocityFt :: R                 -- dt for integral
           -> Mass
           -> Velocity          -- initial velocity
           -> [Time -> Force]   -- list of force functions
           -> Time -> Velocity  -- velocity function
velocityFt dt m v0 fs
    = let fNet t = sum [f t | f <- fs]
          a t = fNet t / m
      in antiDerivative dt v0 a

pedalCoast :: R -> R
pedalCoast t
    = let tCycle = 20
          nComplete :: Int
          nComplete = truncate (t / tCycle)
          remainder = t - fromIntegral nComplete * tCycle
      in if remainder < 10
         then 10
         else 0

pedalCoastGraph :: IO ()
pedalCoastGraph
    = plotFunc [Title "Person pedaling then coasting"
               ,XLabel "Time (s)"
               ,YLabel "Force (n)"
               ,PNG "Force.png"
               ,Key Nothing
               ] [0..40 :: R] (positionFt 0.1 20 0 0 [pedalCoast])

personBikeGraph :: IO ()
personBikeGraph
    = plotFunc [Title "Person pedaling then coasting"
               ,XLabel "Time (s)"
               ,YLabel "Position of Bike (m)"
               ,PNG "personBikeGraph.png"
               ,Key Nothing
               ] [0..40 :: R] (positionFt 0.1 20 0 0 [pedalCoast])
            
-- personVelBikeGraph :: IO ()
-- personVelBikeGraph
--     = plotFunc [Title "Person pedaling then coasting"
--                ,XLabel "Time (s)"
--                ,YLabel "Velocity of Bike (m)"
--                ,PNG "personVelBikeGraph.png"
--                ,Key Nothing
--                ] [0..40 :: R] (velocityFt 0.1 20 0 0 [pedalCoast])