module DescribingMotion where

-- one dimentional movement
-- one of the main advantages of using haskell
-- is that there is almost a one to one mapping between the code and the
-- mathematical notation
-- so if we define the average velocity as 



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

-- recursion over loops

dum :: (Num a, Enum t) => (t -> a) -> t -> t -> a
dum f a b = sum [f i | i <- [a .. b]]

-- testst
test :: Integer
test = dum (^ (2 :: Integer)) 1 10

eulerPi :: (Enum a, Fractional a) => a -> a
eulerPi x = sum [1 / (n^(2 :: Integer)) | n <- [1 .. x]]

-- higher order functions
-- how to think about them
-- very helpful to think about them in  as
-- one input thinking

-- operations as higher order functions (slide)
-- Lets talk about  numerical integration
-- acceleration is the rate of change of velocity
-- acceleraetion is the derivative of velocity
-- velocity is the rate of change of position
-- but what about the converse problem?
-- what if we know the acceleration and we want to find the position
-- we can use the integral to solve this problem
-- the integral is the inverse of the derivative
-- SLIDE introducing integrations page 81
-- lets define the integral
type DigitalIntegration =
  (R -> R) -- function
  -> R -- lower bound
  -> R -- upper bound
  -> R -- result

type Integral = R -> DigitalIntegration


-- using mid point rule
integral :: R -> DigitalIntegration
integral dt f a b =
  sum [f  t * dt | t <- [a + dt/2, a + 3*dt/2 .. b -dt/2]]

-- using the fundamental teorem of calculus
-- we will implement the antiderivative
type Antiderivative =
  R -- initial value
  -> (R -> R) -- function
  -> (R -> R) -- antiderivative function

-- the antiderivative is closely to the integral
-- the antiderivative is the inverse of the derivative

antiderivative :: R -> Antiderivative
antiderivative dt vo a t = vo + integral dt a 0 t

-- now that we have the antiderivative we can use it to find the position
velFromAcc :: R --dt
            -> R -- initial velocity
            -> AccelerationFunction
            -> VelocityFunction
velFromAcc = antiderivative

positionFromVel :: R -- dt
                -> R -- initial position
                -> VelocityFunction
                -> PositionFunction
positionFromVel = antiderivative

-- now that we have these two tools derivative and antiderivative lets graph
-- some models of constant acceleration motion


-- plot the position of a car with constant acceleration

positionCV :: Position -> Velocity -> Time -> Position
positionCV x0 v0 t = v0*t + x0

velocityCA :: Velocity -> Acceleration -> Time -> Velocity
velocityCA v0 a t = v0 + a*t

positionCA :: Position -> Velocity -> Acceleration -> Time -> Position
positionCA x0 v0 a t = x0 + v0*t + 0.5*a*t**2

-- tuples
yRock30 :: Floating a => a -> a
yRock30 t = 30*t - 0.5*9.8*t**2

-- Numerical Integration Redux

oneStep :: R -> -- time step
  (R -> R) -> -- function to integrate
   (R, R) -> -- current value(t,y)
    (R, R) -- updated value(t, y)
oneStep dt f (t, y) = (t + dt, y + f t * dt)

-- updated Integral definition
integral' :: R -> DigitalIntegration
integral' dt f a b = 
  snd $ head $ dropWhile (\(t, _) -> t < b) $ 
  iterate (oneStep dt f) (a, 0)

-- at this point we can improve our model with motion in 3 dimensions 
-- in the first steps quantities where scalars now we will use vectors
