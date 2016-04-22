module Brownrw8.MatrixRotation3D (AxisOfRotation,Radians,rotate)
where

import Data.Matrix

-- Alias Radians == Double
type Radians = Double

-- Axis of rotation, may be x y or z
data AxisOfRotation = XAxis | YAxis | ZAxis

-- Rotate ar axis by d radians
-- Partial application recommended
rotate :: AxisOfRotation -> Radians -> Matrix Double -> Matrix Double
rotate ar r m = case ar of
    XAxis -> (rx r) * m 
    YAxis -> (ry r) * m 
    ZAxis -> (rz r) * m 

-- Rotate x axis by r radians
rx :: Radians -> Matrix Double
rx r =  fromLists [[1,0,0,0]
        ,[0,cos(r),sin(r),0]
        ,[0,(-1) * sin(r),cos(r),0]
        ,[0,0,0,1]]

-- Rotate y axis by r radians
ry :: Radians -> Matrix Double
ry r =  fromLists [[cos(r),0,(-1) * sin(r),0]
        ,[0,1,0,0]
        ,[sin(r),0,cos(r),0]
        ,[0,0,0,1]]
        
-- Rotate z axis by r radians
rz :: Radians -> Matrix Double
rz r =  fromLists [[cos(r),sin(r),0,0]
        ,[(-1) * sin(r),cos(r),0,0]
        ,[0,0,1,0]
        ,[0,0,0,1]]