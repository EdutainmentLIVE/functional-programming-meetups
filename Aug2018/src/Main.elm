module Main exposing (main)

import String
import Html exposing (Html, button, div, text, img)
import Browser
import Html.Events exposing (onClick)
import Html.Attributes exposing (src, height, class)
import CSS

main =
  Browser.sandbox { init = model, view = view, update = update }

-- MODEL

type alias Model = Int

model : Model
model =
  0

-- UPDATE

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg mdl =
  case msg of
    Increment ->
      mdl + 1

    Decrement ->
      mdl - 1

-- VIEW

view : Model -> Html Msg
view mdl =
    div [ CSS.margin5rem ]
        [ buttonsAndInt mdl , sarasDog ]

buttonsAndInt mdl =
    div []
        [ button [ CSS.fontSize3rem, onClick Decrement ] [ text "-" ]
        , div
            [ CSS.fontSize3rem, CSS.backgroundColorRed, CSS.displayInlineBlock ]
            [ text (String.fromInt mdl) ]
        , button [ CSS.fontSize3rem, onClick Increment ] [ text "+" ]
        ]

sarasDog =
    div [ CSS.willNotWorkSadPanda
        , CSS.paddingTop5rem
        , CSS.displayInlineBlock
        ]
        [ img [height 200, src "../assets/cool-dog.jpg"] [] ]

-- view : Model -> Html Msg
-- view mdl =
--     div [ class "margin5rem" ]
--         [ buttonsAndInt mdl , sarasDog ]

-- buttonsAndInt mdl =
--     div []
--         [ button [ onClick Decrement ] [ text "-" ]
--         , div [class "backgroundColorRed", class "displayInlineBlock"] [ text (String.fromInt mdl) ]
--         , button [ onClick Increment ] [ text "+" ]
--         ]

-- sarasDog =
--     div [ class "willNotWorkSadPanda"
--         , class "paddingTop5rem"
--         , class "displayInlineBlock"
--         ]
--         [ img [height 200, src "../assets/cool-dog.jpg"] [] ]