import Animation exposing (Animation)

import Graphics.Element exposing (Element)
import Graphics.Collage exposing (Form)
import Color exposing (Color)
import AnimationFrame
import Mouse
import Signal exposing (Signal)

-- Helpers --

window =
  { width = 1000
  , height = 700
  }
frameCount = 20

startRadius = 20
endRadius = 100
color = RGBA 0 0 255 0

toFormDim : Vector -> Vector
toFormDim {x, y} =
  { x = x - (toFloat window.width / 2)
  , y = (toFloat window.height / 2) - y
  }



radiusAnimation : Animation Float
radiusAnimation =
  Animation.cubic -1 1 frameCount
  |> Animation.map (Animation.lerp startRadius endRadius)



colorAnimation : Animation Color
colorAnimation =
  let
      forward =
        Animation.linear frameCount

      animation =
        Animation.alternate forward


      applyAlpha alpha =
        { color | alpha <- alpha }

  in
      Animation.map (applyAlpha >> toColor) animation




mapAlpha : (Float -> Float) -> Color -> Color
mapAlpha transform color =
  let
      { red, green, blue, alpha } = Color.toRgb color
  in
      Color.rgba red green blue (transform alpha)



-- State --

type alias State =
  { circle : Maybe Circle }

initial : State
initial =
  { circle = Nothing }

type alias Vector =
  { x : Float
  , y : Float
  }


type alias Circle =
  { radius    : Animation Float
  , position  : Vector
  , color     : Animation Color
  }


type alias RGBA =
  { red   : Int
  , green : Int
  , blue  : Int
  , alpha : Float
  }

toColor : RGBA -> Color
toColor {red, green, blue, alpha} =
  Color.rgba red green blue alpha

-- Update --

type Action
  = Click Vector
  | NextFrame


update : Action -> State -> State
update action state =
  case action of
    Click position ->
      { state | circle <- Just (newCircle position) }

    NextFrame ->
      { state | circle <- stepCircle state.circle }


newCircle : Vector -> Circle
newCircle position =
  { position  = position
  , radius    = radiusAnimation
  , color     = colorAnimation
  }

stepCircle : Maybe Circle -> Maybe Circle
stepCircle maybeCircle =
  case maybeCircle of
    Nothing ->
      Nothing

    Just circle ->
      case circle.color.next of
        []  ->
          Nothing

        _   ->
          Just
            { circle | radius <- Animation.forward circle.radius
                     , color  <- Animation.forward circle.color
            }

-- View --

view : State -> Element
view state =
  case state.circle of
    Nothing ->
      Graphics.Collage.collage window.width window.height []

    Just circle ->
      Graphics.Collage.collage window.width window.height
        [ viewCircle circle ]


viewCircle : Circle -> Form
viewCircle circle =
  let pos = toFormDim circle.position
  in
      Graphics.Collage.circle circle.radius.current
      |> Graphics.Collage.filled circle.color.current
      |> Graphics.Collage.move (pos.x, pos.y)


-- Input --

actions : Signal Action
actions =
  Signal.merge clicks nextFrame

clicks : Signal Action
clicks =
  Mouse.position
  |> Signal.sampleOn Mouse.clicks
  |> Signal.map (\(x, y) -> Click (Vector (toFloat x) (toFloat y)))


nextFrame : Signal Action
nextFrame =
  Signal.sampleOn AnimationFrame.frame (Signal.constant NextFrame)

-- Main --


main =
  Signal.map view
    (Signal.foldp update initial actions)
