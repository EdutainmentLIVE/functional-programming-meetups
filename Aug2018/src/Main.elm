module Main exposing (main)

import Browser
import CSS
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, height, src)
import Html.Events exposing (onClick)
import String


main =
    Browser.sandbox { init = model, view = view, update = update }


type alias Model =
    Int


model : Model
model =
    0


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg mdl =
    case msg of
        Increment ->
            mdl + 1

        Decrement ->
            mdl - 1


view : Model -> Html Msg
view mdl =
    div [ CSS.margin5rem ]
        [ buttonsAndInt mdl, sarasDog ]


buttonsAndInt : Model -> Html Msg
buttonsAndInt mdl =
    div []
        [ button [ CSS.fontSize3rem, onClick Decrement ] [ text "-" ]
        , div
            [ CSS.fontSize3rem, CSS.backgroundColorRed, CSS.displayInlineBlock ]
            [ text (String.fromInt mdl) ]
        , button [ CSS.fontSize3rem, onClick Increment ] [ text "+" ]
        ]


sarasDog : Html Msg
sarasDog =
    div
        [ CSS.willNotWorkSadPanda
        , CSS.paddingTop5rem
        , CSS.displayInlineBlock
        ]
        [ img [ height 200, src "../assets/cool-dog.jpg" ] [] ]
