{-# OPTIONS -Wall #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Vectors (
        Vector(..),
    (^+^),
    (^-^),
    (*^),
    (^*),
    (^/),
    (<.>),
    (><),
    PosVec,
    vec,
    magnitude,
    iHat,
    jHat,
    kHat,
    sumV,
    negateV,
    vectorIntegral,
    vectorDerivative,
    zeroV,
    VelocityVecFunction
) where
data Vector = Vector { xComp :: RealNumber,  -- x component
               yComp :: RealNumber,  -- y component
               zComp :: RealNumber  -- z component
} deriving (Eq)

type Time = RealNumber
(^-^) :: Vector -> Vector -> Vector
(*^) :: RealNumber -> Vector -> Vector
(^*) :: Vector -> RealNumber -> Vector
(^/) :: Vector -> RealNumber -> Vector
(<.>) :: Vector -> Vector -> RealNumber
(><) :: Vector -> Vector -> Vector
(^+^) :: Vector -> Vector -> Vector
type RealNumber = Double

-- make slides about vectors 

type PosVec = Vector
type Acceleration = Vector
type Velocity = Vector
--
type VecFunction = RealNumber -> Vector 
type PositionVecFunction = Time -> PosVec
type VelocityVecFunction = Time -> Velocity
type AccelerationVecFunction = Time -> Acceleration


velocityVecFromPosition :: RealNumber -> -- dt
   PositionVecFunction -> VelocityVecFunction
velocityVecFromPosition =
    vectorDerivative
-- from a acceleration function, how can we get the velocity function?
accelerationVecFromVelocity :: RealNumber -> -- dt
  VelocityVecFunction -> AccelerationVecFunction
accelerationVecFromVelocity =
    vectorDerivative
-- -- from a position function, how can we get the Velocity function? // its a scalar but we will handle it as a vec
positionVecFromVelocity :: RealNumber ->  -- dt
  VelocityVecFunction -> PositionVecFunction 
positionVecFromVelocity = 
    undefined
velocityVecFromAcceleration :: RealNumber ->  -- dt
  VelocityVecFunction -> -- initial velocity
  AccelerationVecFunction -> VelocityVecFunction
velocityVecFromAcceleration =
   undefined 

infixl 6 ^+^
(^+^) (Vector ax ay az) (Vector bx by bz) =
    Vector (ax + bx) (ay + by) (az + bz)
infixl 6 ^-^
(^-^) (Vector ax ay az) (Vector bx by bz) =
    Vector (ax - bx) (ay - by) (az - bz)
infixr 7 *^
(*^) s (Vector x y z) =
    Vector (s * x) (s * y) (s * z)
infixl 7 ^*
(^*) (Vector x y z) s =
    Vector (s * x) (s * y) (s * z)
infixr 7 ^/
(^/) (Vector x y z) s =
    Vector (x / s) (y / s) (z / s)
infixr 7 <.>
(<.>) (Vector ax ab az) (Vector bx by bz) =
    ax * bx + ab * by + az * bz
infixl 7 ><
(><) (Vector ax ay az) (Vector bx by bz) =
    Vector (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

magnitude :: Vector -> RealNumber
magnitude v = sqrt (v <.> v)



type VecDerivative = VecFunction -> VecFunction

vectorDerivative :: RealNumber -> VecDerivative


showDouble :: RealNumber -> String
showDouble x
    | x < 0      = "(" ++ show x ++ ")"
    | otherwise  = show x

instance Show Vector where
    show :: Vector -> String
    show (Vector x y z) = "vec " ++ showDouble x ++ " "
                              ++ showDouble y ++ " "
                              ++ showDouble z

vec :: RealNumber -> RealNumber -> RealNumber -> Vector
vec = Vector

iHat :: Vector
iHat = vec 1 0 0

jHat :: Vector
jHat = vec 0 1 0

kHat :: Vector
kHat = vec 0 0 1

zeroV :: Vector
zeroV = vec 0 0 0

negateV :: Vector -> Vector
negateV (Vector x y z) = vec (-x) (-y) (-z)

sumV :: [Vector] -> Vector
sumV = foldr (^+^) zeroV

type VecAntiderivative = RealNumber -> -- we will need an initial state
                       VecFunction -> VecFunction

vecAntiderivative :: RealNumber -> VecAntiderivative
-- vecAntiderivative dt vo  a t = vo + integral dt a 0 t
vecAntiderivative = undefined
-- leave integral definition in undef first 

vectorIntegral :: RealNumber -- step size
             -> (RealNumber -> Vector) -- function to integrate
             -> RealNumber -- lower bound
             -> RealNumber -- upper bound
             -> Vector -- result
         

vectorDerivative dt x t = 
    (x (t + dt / 2) ^-^ x (t - dt / 2)) ^/ dt

vectorIntegral dt f a b =
    let 
        zeroV = vec 0 0 0
        sumV = foldr (^+^) zeroV
    in sumV [f t ^* dt | t <- [a + dt/3, a + 3*dt/2 .. b - dt/2]]    