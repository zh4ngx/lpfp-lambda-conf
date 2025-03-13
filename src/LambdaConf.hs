-- https://docs.google.com/presentation/d/1s5Qmfa1CvMfg0TyvWZTh8MHMXE91JWM5UDlePDsphg8/edit#slide=id.p
module LambdaConf where
import qualified DescribingMotion as Force

-- we want to explore the beaty of Newton's second law, through the lens of s a
-- software engineer using a functional language to express and explore the problem
-- for this we will imagine we are given a working machine that solves a problem of One Body Force.Acceleration
-- The joy comes from the very beginning, when we start naming things, and defining the types
-- Our types will come naturally from the problem we are trying to solve, just by naming things 
-- and expressing math through haskell


-- The strategy we want to follow
-- 1. Define the problem as [OneBodyForce]
-- 2. Create a differential equation from the problem (Use Newton's second law)
-- 3. Solve the differential equation using numerical methods and a state-update function
-- 4. List/Stream of states/solutions


-- 1. Define the problem as [OneBodyForce]
-- The simplest version of newtons second law is the one body force when net body force is constant
-- Most problems are introductory to physics as they do not require solving differencial equations. This means 


-- lets explore motion in term of position, velocity and acceleration
--- in one dimension and with constant acceleration
-- lets make some basic definitions 
type RealNumber   = Double
type Function     = RealNumber -> RealNumber
type Time         = RealNumber
type Position     = RealNumber
type Acceleration = RealNumber
type Velocity     = RealNumber
type Distance     = RealNumber
type TimeInterval = RealNumber

-- lets name base functions 
-- this also define where we will be jumping from and to
type PositionFunction = Time -> Distance
type VelocityFunction = Time -> Velocity
type AccelerationFunction = Time -> Acceleration
-- 

-- most of our 1d Kinetics problems will answering questions like?
-- By looking at the speedometer, how can you tell the distance traveled?
-- from a position function, how can we get the velocity function?
velocityFromPosition :: RealNumber -> PositionFunction -> VelocityFunction
-- from a acceleration function, how can we get the velocity function?
accelerationFromVelocity :: RealNumber -> VelocityFunction -> AccelerationFunction
-- from a position function, how can we get the Velocity function?
positionFromVelocity :: RealNumber ->  -- dt
 RealNumber -> -- initial position
  VelocityFunction -> PositionFunction
velocityFromAcceleration :: RealNumber ->  -- dt
  Velocity -> -- initial velocity
  AccelerationFunction -> VelocityFunction
-- from only looking at the speedometer of a car, how can you tell the distanced traveled? 
-- from only looking at the speedometer of a car, how can you tell the acceleration?


-- Derivatives
type Derivative = Function -> Function

derivative :: RealNumber -> Derivative
derivative dt x t = 
  (x (t + dt / 2 ) - x (t - dt / 2)) / dt

-- conclusion we have introduced the concep of derivate to haskell
-- but also model the problem of kinetics 

-- derivative is now a tool to get to solution through functions


velocityFromPosition = derivative

accelerationFromVelocity = derivative

-- we see now a motion from left to right, from position to velocity to acceleration
-- lets find the inverse function so we can return from acceleration to velocity to position
-- lets define the inverse function of derivative
-- lets call it antiderivative, whichs is o surprise a higher order function
type Antiderivative = RealNumber -> -- we will need an initial state
                       Function -> Function


-- This allow us to move from right to left, from acceleration to velocity to position
-- we have now a complete model of motion in one dimension with constant acceleration
-- fundamental theorem of calculus


positionFromVelocity = antiderivative


velocityFromAcceleration = antiderivative

-- lets expand on the concept of antiderivative using an integral

antiderivative :: RealNumber -> Antiderivative
antiderivative dt vo  a t = vo + integral dt a 0 t
-- leave integral definition in undef first 

type NumericalIntegration = 
    Function -> -- Function to integrate
    RealNumber -> -- Lower bound
    RealNumber -> -- Upper bound
    RealNumber -- RealNumberesult

type Integral = RealNumber -> --dt
 NumericalIntegration
-- Integral using the midpoint rule
integral :: RealNumber -> NumericalIntegration
integral dt f a b = 
    sum [f t * dt | t <- [a + dt / 2, a + 3 * dt / 2 .. b - dt / 2]]



-- lets keep exploring kinetics but in 3d
