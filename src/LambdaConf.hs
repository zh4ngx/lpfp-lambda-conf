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
type R = Double
type Time = R
type Position = R
type Acceleration = R
type Velocity = R
type Distance = R
type TimeInterval = R
type Force = R
type Mass = R

-- lets name base functions 
type Function = R -> R
type PositionFunction = Time -> Distance
type VelocityFunction = Time -> Velocity
type AccelerationFunction = Time -> Acceleration
-- 

-- Derivatives
type Derivative = Function -> Function

-- derivative :: R -> Derivative
-- derivative dt x t = (x (t + dt / 2 ) - x (t - dt / 2)) / dt
derivative :: R -> Derivative
derivative dt x t = (x (t + dt / 2 ) - x (t - dt / 2)) / dt

-- conclusion we have introduced the concep of derivate to haskell
-- but also model the problem of kinetics 

-- derivative is now a tool to get to solution through functions

velFromPosition :: R -> PositionFunction -> VelocityFunction
velFromPosition = derivative

accFromVelocity :: R -> VelocityFunction -> AccelerationFunction
accFromVelocity = derivative

-- we see now a motion from left to right, from position to velocity to acceleration
-- lets find the inverse function so we can return from acceleration to velocity to position
-- lets define the inverse function of derivative
-- lets call it antiderivative, whichs is o surprise a higher order function
type Antiderivative = R -> -- we will need an initial state
                       Function -> Function


-- This allow us to move from right to left, from acceleration to velocity to position
-- we have now a complete model of motion in one dimension with constant acceleration
-- fundamental theorem of calculus

positionFromVelocity :: R ->  -- dt
 R -> -- initial position
  VelocityFunction -> PositionFunction
positionFromVelocity = antiderivative

velocityFromAcceleration :: R ->  -- dt
  Velocity -> -- initial velocity
  AccelerationFunction -> VelocityFunction
velocityFromAcceleration = antiderivative

-- lets expand on the concept of antiderivative using an integral

antiderivative :: R -> Antiderivative
antiderivative dt vo  a t = vo + integral dt a 0 t

type NumericalIntegration = 
    Function -> -- Function to integrate
    R -> -- Lower bound
    R -> -- Upper bound
    R -- Result

type Integral = R -> NumericalIntegration
-- Integral using the midpoint rule
integral :: R -> NumericalIntegration
integral dt f a b = 
    sum [f t * dt | t <- [a + dt / 2, a + 3 * dt / 2 .. b - dt / 2]]


