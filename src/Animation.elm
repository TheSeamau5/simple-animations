module Animation where

import List

type alias Animation a =
  { previous : List a
  , current  : a
  , next     : List a
  }


forward : Animation a -> Animation a
forward animation =
  case animation.next of
    [] ->
      animation

    n :: ns ->
      { previous  = animation.current :: animation.previous
      , current   = n
      , next      = ns
      }


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

alternate : Animation a -> Animation a
alternate animation =
  animation `connect` reverse animation 


constant : a -> Animation a
constant a =
  { previous  = []
  , current   = a
  , next      = []
  }


repeat : Int -> a -> Animation a
repeat n a =
  { previous  = []
  , current   = a
  , next      = List.repeat (n - 1) a
  }


reverse : Animation a -> Animation a
reverse animation =
  { animation | previous <- animation.next
              , next     <- animation.previous
  }

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


loop : Animation a -> Animation a
loop animation =
  case animation.next of
    [] ->
      reset animation

    _ ->
      forward animation


isFinished : Animation a -> Bool
isFinished {next} =
  List.isEmpty next


pause : Int -> Animation a -> Animation a
pause n animation =
  { animation | next <- List.repeat n animation.current ++ animation.next }

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

connect : Animation a -> Animation a -> Animation a
connect first second =
  { previous  = first.previous
  , current   = first.current
  , next      = first.next ++ (List.reverse second.previous) ++ [second.current] ++ second.next
  }

lerp : Float -> Float -> Float -> Float
lerp start end t =
  (1 - t) * start + (t * end)

cubic : Float -> Float -> Int -> Animation Float
cubic a b count =
  linear count
  |> map (\x -> a * (x ^ 3) + b * (x ^ 2) + (1 - a - b) * x)

quadratic : Float -> Int -> Animation Float
quadratic a count =
  linear count
  |> map (\x -> a * (x ^ 2) + (1 - a) * x)

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
