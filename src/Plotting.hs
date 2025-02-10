module Plotting where

import Graphics.Gnuplot.Simple
import Graphics.Gloss

type R = Double

tRange :: [R]
tRange = [0,0.1..5]

yPos :: R -> R -> R -> R -> R
yPos y0 vy0 ay t = y0 + vy0*t + 0.5*ay*t^2



plot1 :: IO ()
plot1 = plotFunc [
    Title "y(t)", XLabel "t", YLabel "y"] tRange (yPos 0 20 (-9.8))

displayMode :: Display
displayMode = InWindow "Plot" (800, 600) (10, 10)

axes :: Picture
axes = Pictures [Color red $ Line [(0,0),(100,0)], 
                 Color green $ Line [(0,0),(0,100)]]
                
displayPlot = display displayMode black axes