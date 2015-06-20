import Animation exposing (Animation)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color
import Window
import Mouse
import AnimationFrame
import Signal exposing (Signal)

type alias Vector =
  { x : Float
  , y : Float
  }

window =
  { width = 1000
  , height = 700
  }

frameCount = 30

type alias State =
  { position : Animation Vector
  }

initial =
  { position = Animation.constant (Vector (toFloat window.width / 2) (toFloat window.height / 2))
  }


type Action
  = UpdatePosition Vector
  | NextFrame


toFormDim : Vector -> Vector
toFormDim {x, y} =
  { x = x - (toFloat window.width / 2)
  , y = (toFloat window.height / 2) - y
  }


ease : Vector -> Vector -> Animation Vector
ease start end =
  let
      --animation = Animation.linear frameCount
      --animation = Animation.cubic -1 1 frameCount
      animation =
        Animation.linear frameCount
        |> Animation.map sqrt


      xs = Animation.map (Animation.lerp start.x end.x) animation

      ys = Animation.map (Animation.lerp start.y end.y) animation

  in
      Animation.map2 Vector xs ys

update : Action -> State -> State
update action state =
  case action of
    UpdatePosition position ->
      { state | position <- ease state.position.current position }

    NextFrame ->
      { state | position <- Animation.play state.position }


view : State -> Element
view {position} =
  let pos = toFormDim position.current
  in
      circle 50
      |> filled Color.red
      |> move (pos.x, pos.y)
      |> flip (::) []
      |> collage window.width window.height


position : Signal Action
position =
  Mouse.position
  |> Signal.map (\(x,y) -> Vector (toFloat x) (toFloat y))
  |> Signal.map UpdatePosition

nextFrame : Signal Action
nextFrame =
  Signal.constant NextFrame
  |> Signal.sampleOn (AnimationFrame.frame)

actions : Signal Action
actions =
  Signal.merge position nextFrame



main =
  Signal.map view
    (Signal.foldp update initial actions)



--main = show "Hello world"
