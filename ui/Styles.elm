module Styles exposing (..)

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

center : Styles
center =
  [ display flex'
  , justifyContent spaceAround
  , margin <| (px 20) ++ " " ++ auto
  , fontSize (px 20)
  , color <| color' (hsl (202 * pi / 180) 0.24 0.27)
  , width (pc 100)
  , backgroundColor (color' darkGrey)
  ]

header : Styles
header =
  [ display flex'
  , paddingLeft (px 10)
  , margin <| (px 20) ++ " " ++ auto
  , fontSize (px 20)
  , color (color' white)
  , width (pc 100)
  , backgroundColor (color' lightOrange)
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
  [ margin <| (px 20) ++ " " ++ auto
  , padding (px 10)
  , fontSize (px 15)
  , backgroundColor (color' blue)
  , color (color' white)
  , width (pc 100)
  ]

delete : Styles
delete =
  [ margin <| (px 20) ++ " " ++ auto
  , padding (px 10)
  , fontSize (px 15)
  , backgroundColor (color' darkPurple)
  , color (color' white)
  , width (pc 100)
  ]

fileName : Styles
fileName =
    [ fontWeight "lighter"
    , fontSize (px 15)
    ]
