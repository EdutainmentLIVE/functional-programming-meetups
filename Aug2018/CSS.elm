module CSS exposing (..)

import Html.Attributes exposing (style)
import Html exposing (Attribute)

backgroundColorRed : Attribute msg
backgroundColorRed = style [ ("backgroundColor", "red") ]

margin1rem : Attribute msg
margin1rem = style [ ("margin", "1rem")]
