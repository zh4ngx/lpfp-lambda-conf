
module Main where
import GHC.Generics (D)

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


main :: IO ()
main = 
  do 
    putStrLn "Hello, Haskell!"
    putStrLn "Hello, Haskell!"
