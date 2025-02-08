
module Main where

-- one dimentional movement
-- one of the main advantages of using haskell
-- is that there is almost a one to one mapping between the code and the
-- mathematical notation
-- so if we define the average velocity as 


-- lets make some basic definitions 
type R = Double
type Time = R
type Acceleration = R
type Velocity = R
type Distance = R
type TimeInterval = R
---

type PositionFunction = Time -> Distance
type VelocityFunction = Time -> Velocity
type AccelerationFunction = Time -> Acceleration

-- average velocity

averageVelocity :: Time -> Time -> PositionFunction -> Velocity
averageVelocity t1 t2 p = (p t2 - p t1) / (t2 - t1)

-- it helps us to think velocity at a single point in time
-- rather than at a time interval
averageVelocity2 :: Time -> TimeInterval -> PositionFunction -> Velocity
averageVelocity2 t dt p = (p (t + dt/2) - p (t - dt/2)) / dt

-- put simple a Derivative is a function that takes a function and returns
-- another function Derivates denotes higher order functions
type Derivative = (R -> R) -> (R -> R)


-- analitical derivative
-- explain derivative here
derivative :: R -> Derivative
derivative dt x t = (x (t + dt / 2 ) - x (t - dt / 2)) / dt


-- Simple excerise
-- car position function described by the following function
carPosition :: PositionFunction
carPosition = cos

-- find the velocity function of the car
velFromPosition :: R -> PositionFunction -> VelocityFunction
velFromPosition = derivative
-- I should list the assumptions of constant velocity

-- find the acceleration function of the car
-- list assumptions for constant acceleration
accFromVelocity :: R -> VelocityFunction -> AccelerationFunction
accFromVelocity = derivative

-- constant acceleration equations 
-- used over and over again in introductury physics

-- Lists!
-- Lists are a fundamental data structure in Haskell
-- sum notation 
dum :: (Num a, Enum t) => (t -> a) -> t -> t -> a
dum f a b = sum [f i | i <- [a .. b]]

-- testst
test :: Integer
test = dum (^(2 :: Integer)) 1 10

eulerPi :: (Enum a, Fractional a) => a -> a
eulerPi x = sum [1 / (n^(2 :: Integer)) | n <- [1 .. x]]

-- higher order functions
-- how to think about them
-- very helpful to think about them in  as
-- one input thinking



main :: IO ()
main =
  do
    putStrLn "Hello, Haskell!"
    putStrLn "Hello, Haskell!"
