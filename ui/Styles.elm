module Styles exposing (..)

import Color exposing (..)
-- reference: http://package.elm-lang.org/packages/elm-lang/core/4.0.3/Color

import Style exposing (..)
-- reference: http://package.elm-lang.org/packages/seanhess/elm-style/1.0.1/
-- reference: http://package.elm-lang.org/packages/seanhess/elm-style/1.0.1/Style
-- reference: https://github.com/seanhess/elm-style/blob/1.0.1/example/MyStyles.elm

type alias Styles = List (String, String)

-- The elm-style library (see the references above!) lets us write our CSS
-- in Elm, similarly to elm-html.
--
-- I know basically nothing about CSS, so I'm assuming the functions
-- used below are familiar to you? They should correspond to CSS properties or whatever.
--

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
  , fontSize (px 20)
  , backgroundColor (color' blue)
  , width (pc 100)
  ]

delete : Styles
delete =
  [ margin <| (px 20) ++ " " ++ auto
  , padding (px 10)
  , fontSize (px 20)
  , backgroundColor (color' darkPurple)
  , width (pc 100)
  ]

fileName : Styles
fileName =
    [ fontWeight "lighter"
    , fontSize (px 15)
    ]
