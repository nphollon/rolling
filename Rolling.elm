module Rolling where

import Signal
import Time

import TimeEvolution

import CubicSpline

mass = 1
g = 100
    
release : CubicSpline.Curve -> Float -> Signal Time.Time -> Signal Float
release fit x0 fps =
  let
    initialState = { x = x0, p = 0 }
  in
    TimeEvolution.evolve laws initialState (extern fit fps)
      |> Signal.map .x

laws =
  { add a b =
    { x = a.x + b.x
    , p = a.p + b.p
    }
  , scale f a =
    { x = f * a.x
    , p = f * a.p
    }
  , force = force
  }

extern fit fps =
  { deltas = fps
  , env = Signal.constant fit
  }

force : CubicSpline.Curve -> State -> State
force fit { x , p } =
  let
    fx = fit x
    s = CubicSpline.slope fx

    dx = p / mass / sqrt (1 + s^2)
    dp = negate mass * g * s / sqrt (1 + s^2)
  in { x = dx, p = dp }


type alias State =
  { x : Float
  , p : Float
  }
