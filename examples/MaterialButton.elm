import Animation exposing (Animation)

import Html exposing (Html, Attribute)
import Html.Attributes
import Html.Events

import AnimationFrame

import Signal exposing (Address)

import Json.Decode as Decode exposing (Decoder)

import List

-- Helpers --
infixl 2 =>

(=>) = (,)

type alias Vector =
  { x : Float
  , y : Float
  }

type alias Color =
  { red   : Int
  , green : Int
  , blue  : Int
  , alpha : Float
  }

toRGBAString : Color -> String
toRGBAString {red, green, blue, alpha} =
  "rgba(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ "," ++ toString alpha ++ ")"



decodeClickLocation : Decoder Vector
decodeClickLocation =
    Decode.object2 Vector
        (Decode.object2 (-)
            (Decode.at ["pageX"] Decode.float)
            (Decode.at ["target", "offsetLeft"] Decode.float)
        )
        (Decode.object2 (-)
            (Decode.at ["pageY"] Decode.float)
            (Decode.at ["target", "offsetTop"] Decode.float)
        )


onClick : Address a -> (Vector -> a) -> Attribute
onClick address constructor =
  Html.Events.on "click" decodeClickLocation (constructor >> Signal.message address)

-- State --

type alias State =
  { label     : String
  , position  : Vector
  , size      : Vector
  , ripple    : Maybe Ripple
  }

init : String -> State
init label =
  { label     = label
  , position  = { x = 100  , y = 100  }
  , size      = { x = 100 , y = 30 }
  , ripple    = Nothing
  }

type alias Ripple =
  { position  : Vector
  , radius    : Animation Float
  , color     : Animation Color
  }

rippleIsFinished : Ripple -> Bool
rippleIsFinished {radius, color} =
  Animation.isFinished radius && Animation.isFinished color

newRipple : Vector -> Vector -> Ripple
newRipple position size =
  { position  = position
  , radius    = radiusAnimation size
  , color     = colorAnimation
  }

frameCount : Int
frameCount = 18

rippleColor : Color
rippleColor =
  { red = 241, green = 196, blue = 15, alpha = 0 }


radiusAnimation : Vector -> Animation Float
radiusAnimation {x, y} =
  let
      maxRadius = max x y * 2

      minRadius = maxRadius / 10

  in
      Animation.cubic -1 1 frameCount
      |> Animation.map (Animation.lerp minRadius maxRadius)



colorAnimation : Animation Color
colorAnimation =
  let
      applyAlpha alpha =
        { rippleColor | alpha <- alpha }

      forward =
        Animation.cubic -1 1 (floor (toFloat frameCount / 2))
        |> Animation.map ((*) 0.7)

      backward =
        Animation.reset (Animation.reverse forward)

  in
      Animation.connect forward backward
      |> Animation.map applyAlpha



-- Update --

type Action
  = Click Vector
  | NextFrame

update : Action -> State -> State
update action state =
  case action of
    Click position ->
      { state | ripple <- Just (newRipple position state.size) }

    NextFrame ->
      { state | ripple <- stepRipple state.ripple }


stepRipple : Maybe Ripple -> Maybe Ripple
stepRipple maybeRipple =
  case maybeRipple of
    Nothing ->
      maybeRipple

    Just ripple ->
      if rippleIsFinished ripple
      then
        Nothing
      else
        Just
          { ripple | radius <- Animation.play ripple.radius
                   , color  <- Animation.play ripple.color
          }




-- View --

view : Address Action -> State -> Html
view address state =
  let
      buttonStyle =
        [ "position"  => "absolute"
        , "width"     => toString state.size.x ++ "px"
        , "height"    => toString state.size.y ++ "px"
        , "top"       => toString state.position.y ++ "px"
        , "left"      => toString state.position.x ++ "px"
        , "cursor"    => "pointer"
        , "overflow"  => "hidden"
        , "outline"   => "none"
        , "background-color"  => "transparent"
        , "border"            => "none"
        , "border-radius"     => "3px"
        , "font-family"       => "Helvetica Neue, Helvetica, Arial, sans-serif"
        , "font-size"         => "14pt"
        , "text-transform"    => "uppercase"
        , "text-align"        => "center"
        , "vertical-align"    => "middle"
        ]


      ripple =
        case state.ripple of
          Nothing ->
            []

          Just r ->
            [ viewRipple r ]

  in
      Html.button
          [ Html.Attributes.style buttonStyle
          , onClick address Click
          ]
          ( Html.text state.label :: ripple )



viewRipple : Ripple -> Html
viewRipple ripple =
  let
      rippleStyle =
        [ "position"          => "absolute"
        , "top"               => toString (ripple.position.y - ripple.radius.current / 2) ++ "px"
        , "left"              => toString (ripple.position.x - ripple.radius.current / 2) ++ "px"
        , "width"             => toString ripple.radius.current ++ "px"
        , "height"            => toString ripple.radius.current ++ "px"
        , "border-radius"     => "50%"
        , "background-color"  => toRGBAString ripple.color.current
        ]
  in
      Html.div
          [ Html.Attributes.style rippleStyle ]
          []


-- Main --

{address, signal} = Signal.mailbox NextFrame

signal' =
  Signal.merge signal
    (Signal.sampleOn AnimationFrame.frame (Signal.constant NextFrame))


main =
  Signal.map (view address)
    (Signal.foldp update (init "Button") signal')
