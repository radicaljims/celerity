module EventStyles exposing (..)

import Color exposing (..)
-- reference: http://package.elm-lang.org/packages/elm-lang/core/4.0.3/Color

import Html exposing (..)
import Html.Attributes as A

import Style exposing (..)

import Bootstrap.Html exposing (..)

type alias Styles = List (String, String)

-- Copied from https://github.com/twopoint718/elmchat/blob/master/src/View.elm
-- Thanks friend!
stylesheet : String -> Html a
stylesheet href =
  node "link"
    [ A.rel "stylesheet"
    , A.href href
    ] []

container : Styles
container =
  [ width (pc 100)
  , height (pc 100)
  ]

fetch : Styles
fetch =
  [ display flex'
  , margin <| (px 20) ++ " " ++ auto
  , fontSize (px 15)
  , width (pc 100)
  ]

header : Styles
header =
  [ display flex'
  , paddingLeft (px 10)
  , margin <| (px 20) ++ " " ++ auto
  , fontSize (px 30)
  , color (color' white)
  , width (pc 100)
  ]

body : Styles
body =
  [ margin <| (px 20) ++ " " ++ auto
  , fontSize (px 20)
  , color (color' white)
  , width (pc 100)
  ]

plainEvent : Styles
plainEvent =
  [ fontWeight "normal"
  , backgroundColor (color' lightOrange)
  ]

addormod : Styles
addormod =
  [
    fontSize (px 15)
  -- , backgroundColor (color' blue)
  -- , color (color' white)
  ]

delete : Styles
delete =
  [
    fontSize (px 15)
  -- , backgroundColor (color' darkPurple)
  -- , color (color' white)
  ]

filePath : Styles
filePath =
    [ fontWeight "lighter"
    , fontSize (px 15)
    ]
