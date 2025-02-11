module Mechanics1d where

import Graphics.Gnuplot.Simple

import NewtonsIILaw (fAir)
import DescribingMotion(R)

type Time     = R
type TimeStep = R
type Mass     = R
type Position = R
type Velocity = R
type Force    = R

type State1D = (Time,Position,Velocity)

newtonSecond1D :: Mass
               -> [State1D -> Force]  -- force funcs
               -> State1D             -- current state
               -> (R,R,R)             -- deriv of state
newtonSecond1D m fs (t,x0,v0)
    = let fNet = sum [f (t,x0,v0) | f <- fs]
          acc = fNet / m
      in (1,v0,acc)