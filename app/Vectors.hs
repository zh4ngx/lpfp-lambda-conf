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
    
-- make slides about vectors 

-- Vec derivative
type VecDerivative = (R -> Vec) -> R -> Vec

data Vec = Vec { xComp :: R  -- x component
               , yComp :: R  -- y component
               , zComp :: R  -- z component
               } deriving (Eq)