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

pedalCoastVelGraph :: IO ()
pedalCoastVelGraph
    = plotFunc [Title "Velocity pedaling then coasting"
               ,XLabel "Time (s)"
               ,YLabel "Velocity(t)"
               ,PNG "VelocityPedaling.png"
               ,Key Nothing
               ] [0..40 :: R] (velocityFt 0.1 20 0 [pedalCoast])

pedalFoceGraph :: IO ()
pedalFoceGraph
    = plotFunc [Title "Person pedaling then coasting"
               ,XLabel "Time (s)"
               ,YLabel "Force (n)"
               ,PNG "Force.png"
               ,Key Nothing
               ] [0..40 :: R] pedalCoast


fAir :: R  -- drag coefficient
     -> R  -- air density
     -> R  -- cross-sectional area of object
     -> Velocity
     -> Force
fAir drag rho area v = - (drag * rho * area * abs v * v / 2)

newtonSecondV :: Mass
              -> [Velocity -> Force]  -- list of force functions
              -> Velocity             -- current velocity
              -> R                    -- derivative of velocity
newtonSecondV m fs v0 = sum [f v0 | f <- fs] / m

updateVelocity :: R                    -- time interval dt
               -> Mass
               -> [Velocity -> Force]  -- list of force functions
               -> Velocity             -- current velocity
               -> Velocity             -- new velocity
updateVelocity dt m fs v0
    = v0 + newtonSecondV m fs v0 * dt

velocityFv :: R                    -- time step
           -> Mass
           -> Velocity             -- initial velocity v(0)
           -> [Velocity -> Force]  -- list of force functions
           -> Time -> Velocity     -- velocity function
velocityFv dt m v0 fs t
    = let numSteps = abs $ round (t / dt)
      in iterate (updateVelocity dt m fs) v0 !! numSteps

bikeVelocity :: Time -> Velocity
bikeVelocity = velocityFv 1 70 0 [const 100,fAir 2 1.225 0.6]

bikeGraph :: IO ()
bikeGraph = plotFunc [Title "Bike velocity"
                     ,XLabel "Time (s)"
                     ,YLabel "Velocity of Bike (m/s)"
                     ,PNG "BikeVelocity1.png"
                     ,Key Nothing
                     ] [0,0.5..60] bikeVelocity

newtonSecondTV :: Mass
               -> [(Time,Velocity) -> Force]  -- force funcs
               -> (Time,Velocity)             -- current state
               -> (R,R)                       -- deriv of state
newtonSecondTV m fs (t,v0)
    = let fNet = sum [f (t,v0) | f <- fs]
          acc = fNet / m
      in (1,acc)

updateTV :: R                           -- time interval dt
         -> Mass
         -> [(Time,Velocity) -> Force]  -- list of force funcs
         -> (Time,Velocity)             -- current state
         -> (Time,Velocity)             -- new state
updateTV dt m fs (t,v0)
    = let (dtdt, dvdt) = newtonSecondTV m fs (t,v0)
      in (t  + dtdt * dt
         ,v0 + dvdt * dt)

statesTV :: R                           -- time step
         -> Mass
         -> (Time,Velocity)             -- initial state
         -> [(Time,Velocity) -> Force]  -- list of force funcs
         -> [(Time,Velocity)]           -- infinite list of states
statesTV dt m tv0 fs
    = iterate (updateTV dt m fs) tv0

velocityFtv :: R                           -- time step
            -> Mass
            -> (Time,Velocity)             -- initial state
            -> [(Time,Velocity) -> Force]  -- list of force funcs
            -> Time -> Velocity            -- velocity function
velocityFtv dt m tv0 fs t
    = let numSteps = abs $ round (t / dt)
      in snd $ statesTV dt m tv0 fs !! numSteps

pedalCoastAir :: [(Time,Velocity)]
pedalCoastAir = statesTV 0.1 20 (0,0)
                [\(t,_) -> pedalCoast t
                ,\(_,v) -> fAir 2 1.225 0.5 v]

pedalCoastAirGraph :: IO ()
pedalCoastAirGraph
    = plotPath [Title "Pedaling and coasting with air"
               ,XLabel "Time (s)"
               ,YLabel "Velocity of Bike (m/s)"
               ,PNG "pedalCoastAirGraph.png"
               ,Key Nothing
               ] (takeWhile (\(t,_) -> t <= 100)
                  pedalCoastAir)

pedalCoastAir2 :: Time -> Velocity
pedalCoastAir2 = velocityFtv 0.1 20 (0,0)
                 [\( t,_v) -> pedalCoast t
                 ,\(_t, v) -> fAir 1 1.225 0.5 v]

velocityCF' :: Mass
            -> Velocity          -- initial velocity
            -> [Force]           -- list of forces
            -> Time -> Velocity  -- velocity function
velocityCF' m v0 fs t = undefined m v0 fs t