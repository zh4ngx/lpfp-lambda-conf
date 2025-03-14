{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Mechanics3d where
import DescribingMotion(R)
import Mechanics1d(Diff(..), TimeStep, Time, scale, RealVectorSpace, NumericalMethod, (+++), solver, rungeKutta4)
import Vectors
    ( Vector, PosVec, (^+^), (^-^), (*^), (^*), (^/), (<.>), (><)
    , vec, sumV, magnitude, zeroV, xComp, yComp, zComp, iHat, jHat, kHat,
    VelocityVecFunction )

import Graphics.Gnuplot.Simple
data ParticleState = ParticleState { mass     :: R
                                   , charge   :: R
                                   , time     :: R
                                   , posVec   :: Vector
                                   , velocity :: Vector }
                     deriving Show


defaultParticleState :: ParticleState
defaultParticleState = ParticleState { mass     = 1
                                     , charge   = 0
                                     , time     = 0
                                     , posVec   = zeroV
                                     , velocity = zeroV }

rockState :: ParticleState
rockState
    = defaultParticleState { mass     = 2                        -- kg
                           , velocity = 3 *^ iHat ^+^ 4 *^ kHat  -- m/s
                           }

-- definition of one body problem
type OneBodyForce = ParticleState -> Vector


data DParticleState = DParticleState { dmdt :: R
                                     , dqdt :: R
                                     , dtdt :: R
                                     , drdt :: Vector
                                     , dvdt :: Vector }
                      deriving Show

newtonSecondPS :: [OneBodyForce]
               -> ParticleState -> DParticleState  -- a differential equation
newtonSecondPS fs st
    = let fNet = sumV [f st | f <- fs]
          m = mass st
          v = velocity st
          acc = fNet ^/ m
      in DParticleState { dmdt = 0    -- dm/dt
                        , dqdt = 0    -- dq/dt
                        , dtdt = 1    -- dt/dt
                        , drdt = v    -- dr/dt
                        , dvdt = acc  -- dv/dt
                        }

-- z direction is toward the sky

-- 

-- z direction is toward the sky
-- assumes SI units
earthSurfaceGravity :: OneBodyForce
earthSurfaceGravity st
    = let g = 9.80665  -- m/s^2
      in (- (mass st * g)) *^ kHat

-- origin is at center of sun
-- assumes SI units
sunGravity :: OneBodyForce
sunGravity (ParticleState m _q _t r _v)
    = let bigG = 6.67408e-11  -- N m^2/kg^2
          sunMass = 1.98848e30  -- kg
      in (- (bigG * sunMass * m)) *^ r ^/ magnitude r ** 3

airResistance :: R  -- drag coefficient
              -> R  -- air density
              -> R  -- cross-sectional area of object
              -> OneBodyForce
airResistance drag rho area (ParticleState _m _q _t _r v)
    = (- (0.5 * drag * rho * area * magnitude v)) *^ v

windForce :: Vector  -- wind velocity
          -> R    -- drag coefficient
          -> R    -- air density
          -> R    -- cross-sectional area of object
          -> OneBodyForce
windForce vWind drag rho area (ParticleState _m _q _t _r v)
    = let vRel = v ^-^ vWind
      in (- (0.5 * drag * rho * area * magnitude vRel)) *^ vRel

uniformLorentzForce :: Vector  -- E
                    -> Vector  -- B
                    -> OneBodyForce
uniformLorentzForce vE vB (ParticleState _m q _t _r v)
    = q *^ (vE ^+^ v >< vB)


eulerCromerPS :: TimeStep        -- dt for stepping
              -> NumericalMethod ParticleState DParticleState
eulerCromerPS dt deriv st
    = let t   = time     st
          r   = posVec   st
          v   = velocity st
          dst = deriv st
          acc = dvdt dst
          v'  = v ^+^ acc ^* dt
      in st { time     = t  +         dt
            , posVec   = r ^+^ v'  ^* dt
            , velocity = v ^+^ acc ^* dt
            }

instance RealVectorSpace DParticleState where
    (+++) :: DParticleState -> DParticleState -> DParticleState
    dst1 +++ dst2
        = DParticleState { dmdt = dmdt dst1  +  dmdt dst2
                         , dqdt = dqdt dst1  +  dqdt dst2
                         , dtdt = dtdt dst1  +  dtdt dst2
                         , drdt = drdt dst1 ^+^ drdt dst2
                         , dvdt = dvdt dst1 ^+^ dvdt dst2
                         }
    scale :: R -> DParticleState -> DParticleState
    scale w dst
        = DParticleState { dmdt = w *  dmdt dst
                         , dqdt = w *  dqdt dst
                         , dtdt = w *  dtdt dst
                         , drdt = w *^ drdt dst
                         , dvdt = w *^ dvdt dst
                         }

instance Diff ParticleState DParticleState where
    shift :: R -> DParticleState -> ParticleState -> ParticleState
    shift dt dps (ParticleState m q t r v)
        = ParticleState (m  +  dmdt dps  * dt)
                        (q  +  dqdt dps  * dt)
                        (t  +  dtdt dps  * dt)
                        (r ^+^ drdt dps ^* dt)
                        (v ^+^ dvdt dps ^* dt)

statesPS :: NumericalMethod ParticleState DParticleState
         -> [OneBodyForce]  -- list of force funcs
         -> ParticleState -> [ParticleState]  --evolver
statesPS method = iterate . method . newtonSecondPS

updatePS :: NumericalMethod ParticleState DParticleState
         -> [OneBodyForce]
         -> ParticleState -> ParticleState
updatePS method = method . newtonSecondPS

positionPS :: NumericalMethod ParticleState DParticleState
           -> [OneBodyForce]  -- list of force funcs
           -> ParticleState   -- initial state
           -> VelocityVecFunction  -- position function
positionPS method fs st t
    = let states = statesPS method fs st
          dt = time (states !! 1) - time (head states)
          numSteps = abs $ round (t / dt)
          st1 = solver method (newtonSecondPS fs) st !! numSteps
      in posVec st1

baseballForces :: [OneBodyForce]
baseballForces
    = let area = pi * (0.074 / 2) ** 2
      in [earthSurfaceGravity
         ,airResistance 0.3 1.225 area]

baseballTrajectory :: R  -- time step
                   -> R  -- initial speed
                   -> R  -- launch angle in degrees
                   -> [(R,R)]  -- (y,z) pairs
baseballTrajectory dt v0 thetaDeg
    = let thetaRad = thetaDeg * pi / 180
          vy0 = v0 * cos thetaRad
          vz0 = v0 * sin thetaRad
          initialState
              = ParticleState { mass     = 0.145
                              , charge   = 0
                              , time     = 0
                              , posVec   = zeroV
                              , velocity = vec 0 vy0 vz0 }
      in trajectory $ zGE0 $
         statesPS (eulerCromerPS dt) baseballForces initialState

zGE0 :: [ParticleState] -> [ParticleState]
zGE0 = takeWhile (\(ParticleState _ _ _ r _) -> zComp r >= 0)

trajectory :: [ParticleState] -> [(R,R)]
trajectory sts = [(yComp r,zComp r) | (ParticleState _ _ _ r _) <- sts]

baseballRange :: R  -- time step
              -> R  -- initial speed
              -> R  -- launch angle in degrees
              -> R  -- range
baseballRange dt v0 thetaDeg
    = let (y,_) = last $ baseballTrajectory dt v0 thetaDeg
      in y

baseballRangeGraph :: IO ()
baseballRangeGraph
    = plotFunc [Title "Range for baseball hit at 45 m/s"
               ,XLabel "Angle above horizontal (degrees)"
               ,YLabel "Horizontal range (m)"
            --    ,PNG "baseballrange.png"
            --    ,Key Nothing
               ] [10,11..80] $ baseballRange 0.01 45


bestAngle :: (R,R)
bestAngle
    = maximum [(baseballRange 0.01 45 thetaDeg,thetaDeg) |
               thetaDeg <- [30,31..60]]


relativityPS :: [OneBodyForce]
             -> ParticleState -> DParticleState  -- a differential equation
relativityPS fs st
    = let fNet = sumV [f st | f <- fs]
          c = 299792458  -- m / s
          m = mass st
          v = velocity st
          u = v ^/ c
          acc = sqrt (1 - u <.> u) *^ (fNet ^-^ (fNet <.> u) *^ u) ^/ m
      in DParticleState { dmdt = 0    -- dm/dt
                        , dqdt = 0    -- dq/dt
                        , dtdt = 1    -- dt/dt
                        , drdt = v    -- dr/dt
                        , dvdt = acc  -- dv/vt
                        }

constantForcePlot :: IO ()
constantForcePlot
    = let year = 365.25 * 24 * 60 * 60  -- seconds
          c = 299792458                 -- m/s
          method = rungeKutta4 1000
          forces = [const (10 *^ iHat)]
          initialState = defaultParticleState { mass = 1 }
          newtonStates = solver method (newtonSecondPS forces) initialState
          relativityStates = solver method (relativityPS forces) initialState
          newtonTVs = [(time st / year, xComp (velocity st) / c)
                           | st <- takeWhile tle1yr newtonStates]
          relativityTVs = [(time st / year, xComp (velocity st) / c)
                               | st <- takeWhile tle1yr relativityStates]
      in plotPaths [Key Nothing
                   ,Title "Response to a constant force"
                   ,XLabel "Time (years)"
                   ,YLabel "Velocity (multiples of c)"
                --    ,PNG "constantForceComp.png"
                   ,customLabel (0.1,1) "mass = 1 kg"
                   ,customLabel (0.1,0.9) "force = 10 N"
                   ,customLabel (0.5,0.7) "Newtonian"
                   ,customLabel (0.8,0.6) "relativistic"
                   ] [newtonTVs,relativityTVs]

tle1yr :: ParticleState -> Bool
tle1yr = (<= 365.25 * 24 * 60 * 60) . time


customLabel :: (R,R) -> String -> Attribute
customLabel (x,y) label
     = Custom "label"
       ["\"" ++ label ++ "\"" ++ " at " ++ show x ++ "," ++ show y]


