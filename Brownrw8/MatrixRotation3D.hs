module Brownrw8.MatrixRotation3D (AxisOfRotation,Degrees,rotate)
where

import Data.Matrix

-- Alias Degrees == Double
type Degrees = Double

-- Axis of rotation, may be x y or z
data AxisOfRotation = XAxis | YAxis | ZAxis

-- Rotate ar axis by d degrees
-- Partial application recommended
rotate :: AxisOfRotation -> Degrees -> Matrix Double -> Matrix Double
rotate ar d m = case ar of
    XAxis -> (rx d) * m 
    YAxis -> (ry d) * m 
    ZAxis -> (rz d) * m 

-- Rotate x axis by d degrees
rx :: Degrees -> Matrix Double
rx d =  fromLists [[1,0,0,0]
        ,[0,cos(d),sin(d),0]
        ,[0,(-1) * sin(d),cos(d),0]
        ,[0,0,0,1]]

-- Rotate y axis by d degrees
ry :: Degrees -> Matrix Double
ry d =  fromLists [[cos(d),0,(-1) * sin(d),0]
        ,[0,1,0,0]
        ,[sin(d),0,cos(d),0]
        ,[0,0,0,1]]
        
-- Rotate z axis by d degrees
rz :: Degrees -> Matrix Double
rz d =  fromLists [[cos(d),sin(d),0,0]
        ,[(-1) * sin(d),cos(d),0,0]
        ,[0,0,1,0]
        ,[0,0,0,1]]