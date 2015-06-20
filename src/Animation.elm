module Animation where
{-| Simple Animation Package.

# Animation Type
@docs Animation

# Play an animation
@docs play, back, loop, pause, reset

# Create an animation
@docs constant, repeat, alternate, reverse, connect

# Query an animation
@docs isFinished, length

# Interpolate
@docs lerp, linear, quadratic, cubic

# Map over an animation
@docs map, map2, andMap

-}

import List


{-| Main Animation Type.
An animation is just a list of frames that is aware of the current frame.
-}
type alias Animation a =
  { previous : List a
  , current  : a
  , next     : List a
  }


{-| Move an animation forward by one frame.
-}
play : Animation a -> Animation a
play animation =
  case animation.next of
    [] ->
      animation

    n :: ns ->
      { previous  = animation.current :: animation.previous
      , current   = n
      , next      = ns
      }


{-| Move an animation back by one frame.
-}
back : Animation a -> Animation a
back animation =
  case animation.previous of
    [] ->
      animation

    p :: ps ->
      { previous  = ps
      , current   = p
      , next      = animation.current :: animation.next
      }


{-| Connect an animation and its reverse end to end, such that playing it
fully ends with the very first frame.
-}
alternate : Animation a -> Animation a
alternate animation =
  animation `connect` reverse animation


{-| Make an animation consisting of a single frame from a value.
-}
constant : a -> Animation a
constant a =
  { previous  = []
  , current   = a
  , next      = []
  }


{-| Make an animation consisting of the same frame repeated `n` times.
-}
repeat : Int -> a -> Animation a
repeat n a =
  { previous  = []
  , current   = a
  , next      = List.repeat (n - 1) a
  }


{-| Reverse an animation at a given point. The past frames become the future
frames and the future frames become the past frames.
-}
reverse : Animation a -> Animation a
reverse animation =
  { animation | previous <- animation.next
              , next     <- animation.previous
  }


{-| Reset an animation by making the current frame be the very first frame.
-}
reset : Animation a -> Animation a
reset animation =
  case List.reverse animation.previous of
    [] ->
      animation

    x :: xs ->
      { previous = []
      , current  = x
      , next     = xs ++ [ animation.current ] ++ animation.next
      }


{-| Either go forward in an animation if there are still future frames, or
reset the animation and play from there.
-}
loop : Animation a -> Animation a
loop animation =
  case animation.next of
    [] ->
      reset animation

    _ ->
      play animation



{-| Return `True` if there are any future frames left.
-}
isFinished : Animation a -> Bool
isFinished {next} =
  List.isEmpty next


{-| Return the number of frames total in an animation.
-}
length : Animation a -> Int
length animation =
  List.length animation.previous + 1 + List.length animation.next

{-| Repeat the current frame `n` many times to simulate pausing for `n` many
frames.
-}
pause : Int -> Animation a -> Animation a
pause n animation =
  { animation | next <- List.repeat n animation.current ++ animation.next }


{-| Map a function over an animation.
-}
map : (a -> b) -> Animation a -> Animation b
map f animation =
  { previous  = List.map f animation.previous
  , current   = f animation.current
  , next      = List.map f animation.next
  }


map2 : (a -> b -> c) -> Animation a -> Animation b -> Animation c
map2 f animationA animationB =
  { previous  = List.map2 f animationA.previous animationB.previous
  , current   = f animationA.current animationB.current
  , next      = List.map2 f animationA.next animationB.next
  }


andMap : Animation (a -> b) -> Animation a -> Animation b
andMap =
  map2 (<|)

{-| Connect two animations end to end. Considers the current frame to be that
of the first animation.
-}
connect : Animation a -> Animation a -> Animation a
connect first second =
  { previous  = first.previous
  , current   = first.current
  , next      = first.next ++ (List.reverse second.previous) ++ [second.current] ++ second.next
  }


{-| Linear interpolation.

    lerp start end ratio

    lerp 0 2 0.5 == 1
-}
lerp : Float -> Float -> Float -> Float
lerp start end t =
  (1 - t) * start + (t * end)


{-| Construct an animation where values go from 0 to 1 where the size of the
intervals are derived from the cubic curve:

    f x = a * (x ^ 3) + b * (x ^ 2) + (1 - a - b) * x

where `a` and `b` are the first two arguments to `cubic`

    cubic a b numberOfIntervals
-}
cubic : Float -> Float -> Int -> Animation Float
cubic a b count =
  linear count
  |> map (\x -> a * (x ^ 3) + b * (x ^ 2) + (1 - a - b) * x)



{-| Construct an animation where values go from 0 to 1 where the size of
the intervals are derived from the quadratic curve:

    f x = a * (x ^ 2) + (1 - a) * x

where `a` is the first argument to `quadratic`

    quadratic a numberOfIntervals
-}
quadratic : Float -> Int -> Animation Float
quadratic a count =
  linear count
  |> map (\x -> a * (x ^ 2) + (1 - a) * x)


{-| Construct an animation where values go from 0 to 1 in regular intervals.

    linear 5 == Animation [] 0 [0.2, 0.4, 0.6, 0.8, 1]
-}
linear : Int -> Animation Float
linear count =
  let
      delta =
        if count <= 1
        then
          1
        else
          1 / (toFloat count)

      next =
        List.repeat count 0
        |> List.indexedMap (\n _ -> delta + (toFloat n) * delta)

  in
      { previous  = []
      , current   = 0
      , next      = next
      }
