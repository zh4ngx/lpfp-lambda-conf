{-# OPTIONS -Wall #-}
module Vectors (
        Vec(..),
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
    vecIntegral,
    vecDerivative,
    zeroV,
    VelocityVecFunction
) where

(^+^) :: Vec -> Vec -> Vec
(^-^) :: Vec -> Vec -> Vec
(*^) :: RealNumber -> Vec -> Vec
(^*) :: Vec -> RealNumber -> Vec
(^/) :: Vec -> RealNumber -> Vec
(<.>) :: Vec -> Vec -> RealNumber
(><) :: Vec -> Vec -> Vec
type RealNumber = Double

-- make slides about vectors 
type Time = RealNumber
type PosVec = Vec
type Acceleration = Vec
type Velocity = Vec
--
type PositionVecFunction = Time -> PosVec
type VelocityVecFunction = Time -> Velocity
type AccelerationVecFunction = Time -> Acceleration


data Vec = Vec { xComp :: RealNumber  -- x component
               , yComp :: RealNumber  -- y component
               , zComp :: RealNumber  -- z component
               } deriving (Eq)


infixl 6 ^+^
(^+^) (Vec ax ay az) (Vec bx by bz) =
    Vec (ax + bx) (ay + by) (az + bz)
infixl 6 ^-^
(^-^) (Vec ax ay az) (Vec bx by bz) =
    Vec (ax - bx) (ay - by) (az - bz)
infixr 7 *^
(*^) s (Vec x y z) =
    Vec (s * x) (s * y) (s * z)
infixl 7 ^*
(^*) (Vec x y z) s =
    Vec (s * x) (s * y) (s * z)
infixr 7 ^/
(^/) (Vec x y z) s =
    Vec (x / s) (y / s) (z / s)
infixr 7 <.>
(<.>) (Vec ax ab az) (Vec bx by bz) =
    ax * bx + ab * by + az * bz
infixl 7 ><
(><) (Vec ax ay az) (Vec bx by bz) =
    Vec (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

magnitude :: Vec -> RealNumber
magnitude v = sqrt (v <.> v)



type VecDerivative = (RealNumber -> Vec) -> RealNumber -> Vec

vecDerivative :: RealNumber -> VecDerivative
vecDerivative dt x t = 
    (x (t + dt / 2) ^-^ x (t - dt / 2)) ^/ dt

showDouble :: RealNumber -> String
showDouble x
    | x < 0      = "(" ++ show x ++ ")"
    | otherwise  = show x

instance Show Vec where
    show (Vec x y z) = "vec " ++ showDouble x ++ " "
                              ++ showDouble y ++ " "
                              ++ showDouble z

vec :: RealNumber -> RealNumber -> RealNumber -> Vec
vec = Vec

iHat :: Vec
iHat = vec 1 0 0

jHat :: Vec
jHat = vec 0 1 0

kHat :: Vec
kHat = vec 0 0 1

zeroV :: Vec
zeroV = vec 0 0 0

negateV :: Vec -> Vec
negateV (Vec x y z) = vec (-x) (-y) (-z)

sumV :: [Vec] -> Vec
sumV = foldr (^+^) zeroV

vecIntegral :: RealNumber -- step size
             -> (RealNumber -> Vec) -- function to integrate
             -> RealNumber -- lower bound
             -> RealNumber -- upper bound
             -> Vec -- result
vecIntegral dt f a b =
    sumV [f t ^* dt | t <- [a + dt/2, a + 3*dt/2 .. b - dt/2]]             

