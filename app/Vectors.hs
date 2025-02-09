module Vectors where


type R = Double

infixl 6 ^+^
(^+^) :: Vec -> Vec -> Vec
(^+^) (Vec ax ay az) (Vec bx by bz) =
    Vec (ax + bx) (ay + by) (az + bz)
infixl 6 ^-^
(^-^) :: Vec -> Vec -> Vec
(^-^) (Vec ax ay az) (Vec bx by bz) =
    Vec (ax - bx) (ay - by) (az - bz)
infixr 7 *^
(*^) :: R -> Vec -> Vec
(*^) s (Vec x y z) =
    Vec (s * x) (s * y) (s * z)
infixl 7 ^*
(^*) :: Vec -> R -> Vec
(^*) (Vec x y z) s =
    Vec (s * x) (s * y) (s * z)
infixr 7 ^/
(^/) :: Vec -> R -> Vec
(^/) (Vec x y z) s =
    Vec (x / s) (y / s) (z / s)
infixr 7 <.>
(<.>) :: Vec -> Vec -> R
(<.>) (Vec ax ab az) (Vec bx by bz) =
    ax * bx + ab * by + az * bz
infixl 7 ><
(><) :: Vec -> Vec -> Vec
(><) (Vec ax ay az) (Vec bx by bz) =
    Vec (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

magnitudeV :: Vec -> R
magnitudeV v = sqrt (v <.> v)


-- make slides about vectors 

-- Vec derivative
type VecDerivative = (R -> Vec) -> R -> Vec

data Vec = Vec { xComp :: R  -- x component
               , yComp :: R  -- y component
               , zComp :: R  -- z component
               } deriving (Eq)

showDouble :: R -> String
showDouble x
    | x < 0      = "(" ++ show x ++ ")"
    | otherwise  = show x

instance Show Vec where
    show (Vec x y z) = "vec " ++ showDouble x ++ " "
                              ++ showDouble y ++ " "
                              ++ showDouble z

vec :: R -> R -> R -> Vec
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
