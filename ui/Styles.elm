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
center : Styles
center =
  [ display flex'
  -- Note that properties get camel cased: justify-content -> justifyContent
  -- That's just part of Elm's style guide
  , justifyContent spaceAround
  -- the '<|' is pipey syntax for function application with the input to the left
  -- so what would otherwise be written f(x) becomes x |> f.
  , margin <| (px 20) ++ " " ++ auto
  , fontSize (px 20)
  -- if you are weirded out by what the hsl is doing, look at the Color reference above
  , color <| color' (hsl (202 * pi / 180) 0.24 0.27)
  , width (pc 100)
  , maxWidth (px 700)
  ]

toast : Styles
toast =
  [ justifyContent spaceAround
  , margin <| (px 20) ++ " " ++ auto
  , fontSize (px 20)
  , backgroundColor (color' green)
  , width (pc 100)
  , maxWidth (px 700)
  ]
