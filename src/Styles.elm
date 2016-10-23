module Styles exposing (cell, buttonGreen)

import Html exposing (Html)
import Html.Attributes exposing (style)

--Constants
blue = "#60B5CC"
green = "#7ED039"
yellow = "#EEAC00"
darkBlue = "#596277"


cell : Int -> Bool -> Html.Attribute a
cell size alive = 
  let
    dim = "px" |> (++) (toString size)
  in
    style
      [ ("width", dim)
      , ("height", dim)
      , ("border", "1px solid black")
      , ("float", "left")
      , ("background-color", if alive then blue else "white")
      , ("cursor", "default")
      ]

buttonGreen =
  style
    [ ("background-color", green)
    ]
