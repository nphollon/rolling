module CubicSpline where

import Array
import List
import Maybe

type Tangent = Instant Float Float Float
type alias Curve = Float -> Tangent
             
defaultTangent = Instant 0 0 0

value (Instant x _ _) = x
slope (Instant _ v _) = v
curvature (Instant _ _ a) = a

fit : List Float -> (Float, Float) -> Curve
fit hs (xStart, xEnd) x =
  let
    n = List.length hs
    dx = (xEnd - xStart) / toFloat n
    splines = offsetSplines dx hs
    splineAt i = Array.get i splines |> Maybe.withDefault (always defaultTangent)
    index = (x - xStart) / dx |> floor |> clamp 0 (n-2)
    offset = x - dx * (toFloat index) - xStart
  in splineAt index offset

offsetSplines : Float -> List Float -> Array.Array Curve
offsetSplines dx hs = 
  combineHeights dx hs
  |> sweep |> backSub
  |> List.map2 (,) hs
  |> makeSplines dx |> Array.fromList

combineHeights : Float -> List Float -> List Float
combineHeights dx h0s = 
  let
    tail = List.tail h0s
    doubleTail = Maybe.andThen tail List.tail
    combine h0 h1 h2 = 6 * (h0 - 2 * h1 + h2) / dx^2
  in case (tail, doubleTail) of
    (Just h1s, Just h2s) -> List.map3 combine h0s h1s h2s
    otherwise -> []

sweep : List Float -> List (Float, Float)
sweep =
  let cd y (cPrev, dPrev) = (1/(4 - cPrev), (y - dPrev)/(4 - cPrev))
  in List.scanl cd (0,0)

backSub : List (Float, Float) -> List Float
backSub =
  let m (c,d) mNext = d - c * mNext
  in List.reverse >> List.scanl m 0 >> List.reverse

makeSplines : Float -> List (Float, Float) -> List Curve
makeSplines dx m = case List.tail m of
  Just mNext -> List.map2 (makeSpline dx) m mNext
  otherwise -> []

makeSpline : Float -> (Float, Float) -> (Float, Float) -> Curve
makeSpline dx (y0, m0) (y1, m1) =
  let
    a = (m1 - m0) / 6 / dx
    b = m0 / 2
    c = (y1 - y0)/dx - (m1 + 2*m0)*dx/6
    d = y0
    elevation x = a*x^3 + b*x^2 + c*x + d
    slope x = 3*a*x^2 + 2*b*x + c
    curvature x = 6*a + 2*b
  in \x -> Instant (elevation x) (slope x) (curvature x)

