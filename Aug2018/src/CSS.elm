module CSS exposing (..)

import Html.Attributes exposing (style)
import Html exposing (Attribute)

backgroundColorRed : Attribute msg
backgroundColorRed = style "backgroundColor" "red"

margin5rem : Attribute msg
margin5rem = style "margin" "5rem"

displayInlineBlock : Attribute msg
displayInlineBlock = style "display" "inline-block"

willNotWorkSadPanda : Attribute msg
willNotWorkSadPanda = style "asdfasdf" "as;kj;akj"

paddingTop5rem : Attribute msg
paddingTop5rem = style "padding-top" "5rem"

--------------

fontSize3rem : Attribute msg
fontSize3rem = style "font-size" "3rem"

paddingX2rem : Attribute msg
paddingX2rem = style "padding" "0 2rem"