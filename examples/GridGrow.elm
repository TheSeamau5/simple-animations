import Animation exposing (Animation)

import Html exposing (Html)
import Html.Attributes
import Html.Events

import AnimationFrame

import Signal exposing (Address)

import Debug

import List

-- Helpers --

infixl 2 =>

(=>) = (,)

toRgbaString : Rgba -> String
toRgbaString {red, green, blue, alpha} =
  "rgba(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ "," ++ toString alpha ++ ")"


window =
  { size =
      { x = 500
      , y = 500
      }
  }

-- Animations --

vectorAnimation : Vector -> Vector -> Animation Vector
vectorAnimation start end =
  let
      animation =
        --Animation.linear 20
        --|> Animation.map sqrt
        Animation.cubic -1 1 20

      xs =
        animation
        |> Animation.map (Animation.lerp start.x end.x)

      ys =
        animation
        |> Animation.map (Animation.lerp start.y end.y)
  in
      Animation.map2 Vector xs ys

-- State --

type alias Vector =
  { x : Float , y : Float }

origin =
  { x = 0 , y = 0 }

type alias Rgba =
  { red   : Int
  , green : Int
  , blue  : Int
  , alpha : Float
  }


type alias Box =
  { position  : Vector
  , size      : Vector
  , color     : Rgba
  }

type alias GridPage =
  { boxes : List Box }


initialGridPage =
  { boxes =
      [ { position =
            { x = 0
            , y = 0
            }
        , size =
            { x = 100
            , y = 100
            }
        , color =
            { red   = 255
            , green = 0
            , blue  = 0
            , alpha = 1
            }
        }
      , { position =
            { x = 100
            , y = 0
            }
        , size =
            { x = 100
            , y = 100
            }
        , color =
            { red   = 0
            , green = 255
            , blue  = 0
            , alpha = 1
            }
        }
      , { position =
            { x = 200
            , y = 0
            }
        , size =
            { x = 100
            , y = 100
            }
        , color =
            { red   = 0
            , green = 0
            , blue  = 255
            , alpha = 1
            }
        }
      ]
  }


type alias ArticlePage =
  { position  : Animation Vector
  , size      : Animation Vector
  , color     : Rgba
  , isShown   : Bool
  }


newArticlePage : Box -> ArticlePage
newArticlePage box =
  { position  = vectorAnimation box.position origin
  , size      = vectorAnimation box.size window.size
  , color     = box.color
  , isShown   = False
  }


type alias State =
  { gridPage    : GridPage
  , articlePage : Maybe ArticlePage
  }


initial =
  { gridPage    = initialGridPage
  , articlePage = Nothing
  }


-- Update --

type BoxAction
  = Click Box

type ArticleAction
  = Close

type Action
  = Grid BoxAction
  | Article ArticleAction
  | NextFrame


update : Action -> State -> State
update action state =
  case action of
    Grid boxAction ->
      applyBoxAction boxAction state

    Article articleAction ->
      applyArticleAction articleAction state

    NextFrame ->
      nextFrame state


applyBoxAction : BoxAction -> State -> State
applyBoxAction boxAction state =
  case boxAction of
    Click box ->
      case state.articlePage of
        Nothing ->
          let
              page = newArticlePage box

          in
              { state | articlePage <- Just ({ page | isShown <- True })
              }

        Just _ ->
          state


applyArticleAction : ArticleAction -> State -> State
applyArticleAction action state =
  case action of
    Close ->
      case state.articlePage of
        Nothing ->
          state

        Just page ->
          if List.isEmpty page.position.previous && List.isEmpty page.size.previous
          then
            { state | articlePage <- Nothing }
          else
            { state | articlePage <- Just (stepArticle { page | isShown <- False }) }

nextFrame : State -> State
nextFrame state =
  case state.articlePage of
    Nothing -> state
    Just article ->
      if article.isShown
      then
        { state | articlePage <- Just (stepArticle article) }
      else
        if List.isEmpty article.position.previous && List.isEmpty article.size.previous
        then
          { state | articlePage <- Nothing }
        else
          { state | articlePage <- Just (stepArticle article) }


stepArticle : ArticlePage -> ArticlePage
stepArticle article =
  if article.isShown
  then
    { article | position  <- Animation.play article.position
              , size      <- Animation.play article.size
    }
  else
    { article | position  <- Animation.back article.position
              , size      <- Animation.back article.size
    }

-- View --

view : Address Action -> State -> Html
view address state =
  let
      containerStyle =
          []

      articleAddress =
        Signal.forwardTo address Article

      gridAddress =
        Signal.forwardTo address Grid

      articlePage =
        case state.articlePage of
          Nothing ->
            []
          Just page ->
            [ viewArticlePage articleAddress page ]

  in
      Html.div
          [ Html.Attributes.style containerStyle ]
          ( viewGridPage gridAddress state.gridPage :: articlePage )

viewArticlePage : Address ArticleAction -> ArticlePage -> Html
viewArticlePage address page =
  let
      pageContainerStyle =
          [ "position"  => "absolute"
          , "top"       => toString page.position.current.y ++ "px"
          , "left"      => toString page.position.current.x ++ "px"
          , "width"     => toString page.size.current.x ++ "px"
          , "height"    => toString page.size.current.y ++ "px"
          , "background-color" => toRgbaString page.color
          , "z-index" => "1"
          ]
  in
      Html.div
          [ Html.Attributes.style pageContainerStyle ]
          [ Html.button
                [ Html.Events.onClick address Close ]
                [ Html.text "CLOSE" ]
          ]

viewGridPage : Address BoxAction -> GridPage -> Html
viewGridPage address page =
  let
      pageContainerStyle =
          []

  in
      Html.div
          [ Html.Attributes.style pageContainerStyle ]
          ( List.indexedMap (viewBox address) page.boxes )

viewBox : Address BoxAction -> Int -> Box -> Html
viewBox address n box =
  let
      boxStyle =
          [ "position"  => "absolute"
          , "top"       => toString box.position.y ++ "px"
          , "left"      => toString box.position.x ++ "px"
          , "width"     => toString box.size.x ++ "px"
          , "height"    => toString box.size.y ++ "px"
          , "background-color" => toRgbaString box.color
          , "cursor"    => "pointer"
          ]
  in
      Html.div
          [ Html.Attributes.style boxStyle
          , Html.Events.onClick address (Click box)
          ]
          []


-- Inputs --
{ address, signal } = Signal.mailbox NextFrame

signal' =
  Signal.merge signal
    (Signal.sampleOn AnimationFrame.frame (Signal.constant NextFrame))

-- Main --
main =
  Signal.map (view address)
    (Signal.foldp update initial signal')
