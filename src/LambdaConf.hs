-- https://docs.google.com/presentation/d/1s5Qmfa1CvMfg0TyvWZTh8MHMXE91JWM5UDlePDsphg8/edit#slide=id.p
module LambdaConf where
import qualified DescribingMotion as Force
import Prelude hiding (Integral)
import Vectors
import Mechanics3d (rockState)
import qualified Control.Applicative as Vector

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
type Time         = RealNumber
type Position     = RealNumber
type Acceleration = RealNumber
type Velocity     = RealNumber

type DT = RealNumber --dt

type Function     = RealNumber -> RealNumber

type PositionFunction = Time -> Position

type VelocityFunction = Time -> Velocity

type AccelerationFunction = Time -> Acceleration


-- 

-- most of our 1d Kinetics problems will answering questions like?
-- imagine you are travelling in a car, every second you query the speed take note
-- and create a function that describes the speed of the car
-- from the speed function, how can you tell the acceleration?, distanced traveled? etc
-- By looking at the speedometer, how can you tell the distance traveled?
-- from a position function, how can we get the velocity function?

-- from a acceleration function, how can we get the velocity function?

-- from a position function, how can we get the Velocity function?



accelerationFromPosition :: DT -> -- dt
  PositionFunction -> AccelerationFunction
accelerationFromPosition dt posFunc =
  accelerationFromVelocity dt (velocityFromPosition dt posFunc)

-- from only looking at the speedometer of a car, how can you tell the distanced traveled? 
-- from only looking at the speedometer of a car, how can you tell the acceleration?


-- questions: Why is the area under the curve of the speed function equal to the distance traveled?

-- 

-- Derivative

type Derivative = Function -> Function

derivative :: Time -> Derivative

derivative dt x t = 
  (x (t + dt / 2 ) - x (t - dt / 2)) / dt

antiderivative dx fo a x = 
  fo + integral dx a 0 x



-- conclusion we have introduced the concep of derivate to haskell
-- but also model the problem of kinetics 

-- derivative is now a tool to get to solution through functions

velocityFromPosition :: Time -> -- dt
   PositionFunction -> VelocityFunction

velocityFromPosition = derivative

accelerationFromVelocity :: Time -> -- dt
  VelocityFunction -> AccelerationFunction

accelerationFromVelocity = derivative

positionFromVelocity :: Time ->  -- dt
 Position -> -- initial position
  VelocityFunction -> PositionFunction

positionFromVelocity = antiderivative

velocityFromAcceleration :: Time ->  -- dt
  Velocity -> -- initial velocity
  AccelerationFunction -> VelocityFunction

velocityFromAcceleration = antiderivative


-- we see now a motion from left to right, from position to velocity to acceleration
-- lets find the inverse function so we can return from acceleration to velocity to position
-- lets define the inverse function of derivative
-- lets call it antiderivative, whichs is o surprise a higher order function

-- This allow us to move from right to left, from acceleration to velocity to position
-- we have now a complete model of motion in one dimension with constant acceleration
-- fundamental theorem of calculus




type Antiderivative = 
  RealNumber -> -- initial value
  Function -> Function

antiderivative :: RealNumber -> Antiderivative




type NumericalIntegration = 
    Function -> -- Function to integrate
    Time -> -- Lower bound
    Time -> -- Upper bound
    RealNumber -- RealNumberesult

integral :: Time -> --dt
 NumericalIntegration


integral dt f a b = 
    sum [
      f t * dt | -- rectangle area = f(t) * dt
      t <- [a + dt / 2, a + 3 * dt / 2 .. b - dt / 2] -- stream of time at midpoint
      ]



-- lets keep exploring kinetics but in 3d
type Mass = RealNumber
type Force = RealNumber


newtonSecondLaw :: Mass
              -> [Velocity -> Force]  -- list of force functions
              -> Velocity             -- current velocity
              -> Velocity           -- derivative of velocity

newtonSecondLaw m fs v0 = 

  sum [f v0 | f <- fs] / m

updateVelocity :: Time           -- time interval dt
               -> Mass
               -> [Velocity -> Force]  -- list of force functions
               -> Velocity             -- current velocity
               -> Velocity             -- new velocity
               
updateVelocity dt m fs v0
    = v0 + newtonSecondLaw m fs v0 * dt
  
type ForceFunctionCF _' = _' -> Force 
type ForceFunctionTimeDep = Time -> Force 
type ForceFunctionVelocityDep = Velocity -> Force 

data ParticleState = ParticleState { mass     :: RealNumber
                                   , charge   :: RealNumber
                                   , time     :: RealNumber
                                   , position   :: Vector
                                   , velocity :: Vector }
                     deriving Show

type OneBodyForce = 
  ParticleState -> Vector

val :: const -> Vector
val = const (10 *^ iHat)

rocketState :: ParticleState
rocketState
    = ParticleState { mass     = 2         -- kg
                    , charge   = 0         -- C
                    , time     = 0         -- s
                    , position = 0 *^ iHat -- m
                    , velocity = 0 *^ iHat -- m/s
                    }