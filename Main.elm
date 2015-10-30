module Main where

import AnimationFrame
import Array
import Color
import Graphics.Collage as Graphics exposing (defaultLine)
import Graphics.Element as Layout
import List
import Signal

import CubicSpline
import Rolling

type alias Point = (Float, Float)

main = 
  Rolling.release curve 281 AnimationFrame.frame
    |> Signal.map draw


draw : Float -> Layout.Element
draw x =
  let
    marble =
      ball 4 curve x

    ground =
      curve >> CubicSpline.value
        |> terrain 
        |> drawGround
  in
    Graphics.collage 800 400 [ marble, ground ]


curve : CubicSpline.Curve
curve =
  let
    keyHeights =
      [ 100, 50, 0, 7
      , 30, 35, 80, 73
      , -70, -100, -85, -40
      , -20, 50
      ]
  in
    CubicSpline.fit keyHeights xRange


xRange : (Float, Float)
xRange =
  (-300, 300)


resolution : Int
resolution =
  100


drawGround : List Point -> Graphics.Form
drawGround pts =
  Graphics.path pts
    |> Graphics.traced (Graphics.solid Color.darkGreen)

       
terrain : (Float -> Float) -> List Point
terrain z = 
  let
    (start, end) =
      xRange
                   
    coords x =
      (x, z x)
      
    toX i =
      (i ./. resolution) * (end - start) + start
  in
    Array.initialize resolution (toX >> coords)
      |> Array.toList

    
ball : Float -> CubicSpline.Curve -> Float -> Graphics.Form
ball r fit x =
  let
    z = fit x |> CubicSpline.value
    m = fit x |> CubicSpline.slope
    offset = r / sqrt (m^2 + 1)
    u = x - m*offset
    v = z + offset
  in Graphics.circle r |> Graphics.outlined (Graphics.solid Color.darkBlue) |> Graphics.move (u,v)


(./.) : Int -> Int -> Float
a ./. b =
  (toFloat a) / (toFloat b)
